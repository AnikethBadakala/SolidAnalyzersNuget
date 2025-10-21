using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Diagnostics;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;


namespace SolidAnalyzersNuget
{
    //LSP violations are semantic.
    [DiagnosticAnalyzer(LanguageNames.CSharp)]
    public class SolidAnalyzer : DiagnosticAnalyzer
    {
        //This is the unique ID of your rule
        public const string lspDiagnosticId = "LSP001";

        //the information of rule
        //desc of diagnosis
        //A DiagnosticDescriptor describes a kind of issue (a rule) that your analyzer can report.
        //Think of it as metadata about your warning/error.
        private static readonly DiagnosticDescriptor Rule = new DiagnosticDescriptor(
           lspDiagnosticId,
           "LSP Violation: Derived method throws exception not in base method's contract.",
           "Method '{0}' in derived type '{1}' throws an exception '{2}' not covered by base method '{3}' contract.",
           "SOLID Principles",
           DiagnosticSeverity.Warning,
           isEnabledByDefault: true);             

        //Every analyzer must declare which diagnostic rules it supports.
        //Roslyn uses this to:
        //Tell IDEs(like Visual Studio) what kinds of issues to expect,
        //Display them in the “Analyzers” window
        //Roslyn analyzers are multi-threaded and must be thread-safe.
        //That’s why the analyzer framework uses immutable data structures — objects that cannot change after creation
        //ImmutableArray<DiagnosticDescriptor> means: “Here’s a fixed list of all diagnostics this analyzer might ever report.
        //how rosyln uses it in background
        //When Visual Studio or dotnet build runs analyzers:
        //It queries each analyzer’s SupportedDiagnostics property.
        //It registers those rules with the analysis engine.
        //Whenever your analyzer reports a diagnostic with that ID (LSP001), Roslyn checks that it’s in this list.

        //these are the diagnostics (warnings/errors) my analyzer might report.
        //SupportedDiagnostics says this analyzer can produce warnings with ID LSP001 and category SOLID Principles.”
        public override ImmutableArray<DiagnosticDescriptor> SupportedDiagnostics { get { return ImmutableArray.Create(Rule); } }//to tell the compiler what issue to expect

        //when the code control reaches the analyzers and when the compiler comes to our analyzer, it tells the compiler what code it should check
        //when and how your analyzer should run those rules. at method find
        public override void Initialize(AnalysisContext context)
        {
            //Ignore auto-generated files (so we don’t analyze code like designer files or EF models).
            context.ConfigureGeneratedCodeAnalysis(GeneratedCodeAnalysisFlags.None);

            //Allow the analyzer to run in parallel on multiple files for faster performance.
            context.EnableConcurrentExecution();

            //Does this method override another one?” or “What’s its return type?
            //to find what to analyze,semantic understanding (e.g., base/derived method relationship)
            context.RegisterSymbolAction(AnalyzeMethodSymbol, SymbolKind.Method);

            //Tell Roslyn to call AnalyzeMethodBody every time it finds a method in the source code           
            //to inspect the code Does this method contain a throw statement?
            //context.RegisterSyntaxNodeAction(AnalyzeMethodBody, SyntaxKind.MethodDeclaration);
        }

        //RegisterSymbolAction tells Roslyn:
        //“Whenever you see a symbol of kind Method in the compilation, call this function.”

        //is called for every method symbol in the project.
        //It checks if a derived (child) class method throws new exceptions that the base class method does not — which violates LSP.
        //Roslyn passes a SymbolAnalysisContext, which gives you access to a single method (symbol).
        private static void AnalyzeMethodSymbol(SymbolAnalysisContext context)
        {
            //It contains semantic information about that method,name, param, return type,if its overriding other method, access modifiers
            //methodSymbol as a “metadata object” for a method:
            //Convert the generic symbol into a method symbol so you can access properties like parameters, return type, and overridden methods.
            var methodSymbol = (IMethodSymbol)context.Symbol;

            // Only analyze if it's an override
            //This means even though Roslyn calls AnalyzeMethodSymbol for every method, your logic only continues for methods that override a base method.
            if (methodSymbol.OverriddenMethod is null)
                return;

            //Get the base method that this current method overrides (so we can compare their behavior).
            var baseMethod = methodSymbol.OverriddenMethod;

            var baseSemanticModel = context.Compilation.GetSemanticModel(baseMethod.DeclaringSyntaxReferences.First().SyntaxTree);


            // Gather allowed exception types from base method
            var allowedExceptions = new HashSet<ITypeSymbol>(
            baseMethod
         .DeclaringSyntaxReferences
         .Select(r => r.GetSyntax())
         .OfType<MethodDeclarationSyntax>()
         .SelectMany(m => m.DescendantNodes().OfType<ThrowStatementSyntax>())
         .Select(t => GetThrownType(baseSemanticModel, t))
         .Where(t => t != null),
            SymbolEqualityComparer.Default);


            //Now we look at the derived method’s code to check what exceptions it throws.
            foreach (var syntaxRef in methodSymbol.DeclaringSyntaxReferences)
            {
                //syntaxRef.GetSyntax() returns a SyntaxNode corresponding to the method symbol.
                //We then check: “Is this node a MethodDeclarationSyntax ?”
                //Roslyn recommends always checking the type because GetSyntax() returns SyntaxNode.
                //You cannot assume all SyntaxNodes are MethodDeclarationSyntax(even if symbol says it’s a method).
                //eg: A partial method may have only a declaration in one place and implementation elsewhere.
                //eg: A method coming from a compiled DLL → no syntax tree → GetSyntax() may fail or return null.

                var derivedMethod = syntaxRef.GetSyntax() as MethodDeclarationSyntax;
                if (derivedMethod == null)
                    continue;

                //Get all throw statements inside the derived method,
                //returns a collection of two ThrowStatementSyntax nodes inside of each derived class:
                //throwStatements = [
                //SyntaxNode(for "throw new ArgumentNullException("data");"),
                //SyntaxNode(for "throw new InvalidCastException("Derived error");")
                //]
                //Each is a Roslyn syntax object representing that exact line of code — it knows where in the file it came from, but not what it means yet.
                var throwStatements = derivedMethod.DescendantNodes().OfType<ThrowStatementSyntax>();

                //a semantic model to help understand the type of each thrown exception.
                //Creates a semantic model that can tell you the meaning behind syntax — e.g. what type a symbol or expression refers to.
                //The SyntaxTree just tells you what’s written.
                //The SemanticModel tells you what it represents in terms of types, symbols, etc.
                //For throw new ArgumentNullException("data"); → System.ArgumentNullException
                //For throw new InvalidCastException("Derived error"); → System.InvalidCastException

                //A model object
                //Creates a “semantic model” to interpret syntax
                var derivedSemanticModel = context.Compilation.GetSemanticModel(derivedMethod.SyntaxTree);

                //Loops through every throw statement found in the derived (child) method.
                foreach (var throwStmt in throwStatements)
                {
                    //uses the sematicmodel to find what’s being thrown
                    //we are passing a throw statemnt from the list for each iteration and also the semantic model
                    //semanticModel is the tool we use to understand the code’s meaning, not just its text.
                    var thrownType = GetThrownType(derivedSemanticModel, throwStmt);
                    if (thrownType == null) continue;


                    //compares the throwntype from above with all the available list of allowedexceptions in the base class 
                    //returns true if both ex types are different
                    if (!allowedExceptions.Any(a => SymbolEqualityComparer.Default.Equals(a, thrownType)))
                    {
                        //creates a warning message for the analyzer
                        //A specific instance of a diagnostic — a real warning you want to report right now.
                        //throwStmt.GetLocation() = tells VS where in the code the problem is
                        var diag = Diagnostic.Create(Rule, throwStmt.GetLocation(),
                            methodSymbol.Name, thrownType.Name);

                        //This actually reports the warning to Visual Studio (or compiler).
                        context.ReportDiagnostic(diag);
                    }
                }
            }
        }

        //eg: throw new InvalidOperationException("Something went wrong");
        //Roslyn gives you a syntax node for this line — that’s throwStmt
        //throwStmt = the whole line
        //throwStmt.Expression = new InvalidOperationException("Something went wrong")
        //
        private static ITypeSymbol GetThrownType(SemanticModel model, ThrowStatementSyntax throwStmt)
        {
            //Checks if the throw statement is creating a new object (e.g. new Exception()).
            //Check if the thing being thrown is created using the new keyword.”
            //eg: throw new InvalidOperationException("Error");
            //throwStmt.Expression = new InvalidOperationException("Error")
            //That is an ObjectCreationExpressionSyntax, so this if condition is true.
            //eg: throw ex;
            //then throwStmt.Expression = ex, which is not an object creation — so this if fails.
            if (throwStmt.Expression is ObjectCreationExpressionSyntax obj)
                return model.GetTypeInfo(obj).Type;
            return null;
        }

    }
}
