using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace SolidAnalyzersNuget.CodeFixes
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(SolidAnalyzersNugetCodeFixProvider)), Shared]
    public class SolidAnalyzersNugetCodeFixProvider : CodeFixProvider
    {
        // Define the resource titles (assuming CodeFixResources.RemoveExceptionTitle and ReplaceExceptionTitle exist)
        // If your resources are named CodeFixResources, use that namespace/class name.
        private static readonly string RemoveTitle = CodeFixResources.RemoveExceptionTitle;
        private static readonly string ReplaceTitle = CodeFixResources.ReplaceExceptionTitle;
        // 1. Tell Roslyn which diagnostics (rules) this fix provider can handle.
        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            // NOTE: Assumes SolidAnalyzer is visible here. If not, use the full path or alias.
            get { return ImmutableArray.Create(SolidAnalyzersNuget.SolidAnalyzer.lspDiagnosticId, SolidAnalyzersNuget.SolidAnalyzer.srpDiagnosticId); }
        }

        public sealed override FixAllProvider GetFixAllProvider()
        {
            return WellKnownFixAllProviders.BatchFixer;
        }

        // 2. Register the fix action(s) for the detected diagnostic.
        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);
            var diagnostic = context.Diagnostics.First();
            if (diagnostic.Id == SolidAnalyzersNuget.SolidAnalyzer.srpDiagnosticId)
            {
                // Find the declaration node (Parent class in this case)
                var typeDeclaration = root.FindNode(diagnostic.Location.SourceSpan).AncestorsAndSelf()
                                             .OfType<ClassDeclarationSyntax>()
                                             .FirstOrDefault();

                if (typeDeclaration != null)
                {
                    context.RegisterCodeFix(
                        CodeAction.Create(
                            // Use a title that clearly describes the required action
                            title: $"Refactor '{typeDeclaration.Identifier.Text}' (SRP Violation)",
                            // Create a solution that just adds a comment as a simple fix placeholder
                            createChangedDocument: c => AddRefactoringComment(context.Document, typeDeclaration, c),
                            equivalenceKey: "RefactorSRP"
                        ),
                        diagnostic
                    );
                }
                return;
            }

            // *** IMPORTANT FIX: Use FindNode for reliable location of the entire statement ***
            var diagnosticSpan = diagnostic.Location.SourceSpan;
            var throwStatement = root.FindNode(diagnosticSpan) as ThrowStatementSyntax;

            if (throwStatement == null)
                return;

            // --- A. Fix 1: Remove the Throw Statement ---
            var removeAction = CodeAction.Create(
                title: RemoveTitle,
                createChangedDocument: c => RemoveThrowStatement(context.Document, root, throwStatement, c),
                equivalenceKey: "RemoveLSPViolation" // Specific key
            );
            context.RegisterCodeFix(removeAction, diagnostic);

            // --- B. Fix 2: Replace with NotImplementedException ---
            var replaceAction = CodeAction.Create(
                title: ReplaceTitle,
                createChangedDocument: c => ReplaceWithNotImplemented(context.Document, throwStatement, c),
                equivalenceKey: "ReplaceLSPViolation" // Specific key
            );
            context.RegisterCodeFix(replaceAction, diagnostic);
        }

        // 3. The method that creates the new (fixed) document for REMOVING the throw statement.
        private static Task<Document> RemoveThrowStatement(
            Document document,
            SyntaxNode root,
            ThrowStatementSyntax throwStatement,
            CancellationToken cancellationToken)
        {
            var newRoot = root.RemoveNode(throwStatement, SyntaxRemoveOptions.KeepNoTrivia);
            return Task.FromResult(document.WithSyntaxRoot(newRoot));
        }

        // 4. NEW: Method to Replace the Exception
        private static Task<Document> ReplaceWithNotImplemented(
            Document document,
            ThrowStatementSyntax throwStatement,
            CancellationToken cancellationToken)
        {
            // Create the expression: 'new NotImplementedException()'
            var newException = SyntaxFactory.ObjectCreationExpression(
                SyntaxFactory.IdentifierName("NotImplementedException"),
                SyntaxFactory.ArgumentList(),
                null);

            // Create the new throw statement: 'throw new NotImplementedException();'
            var newThrowStatement = throwStatement
                .WithExpression(newException)
                .NormalizeWhitespace();

            // Get the root and replace the old throw statement with the new one
            var oldRoot = document.GetSyntaxRootAsync(cancellationToken).Result;
            var newRoot = oldRoot.ReplaceNode(throwStatement, newThrowStatement);

            return Task.FromResult(document.WithSyntaxRoot(newRoot));
        }

        private async Task<Document> ExtractCalculationMethodsAsync(
            Document document,
            TypeDeclarationSyntax typeDeclaration,
            CancellationToken cancellationToken)
        {
            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);

            // Find calculation methods
            var calculationMethods = typeDeclaration.Members
                .OfType<MethodDeclarationSyntax>()
                .Where(m => m.Identifier.Text.Contains("Calculate"))
                .ToList();

            if (!calculationMethods.Any())
                return document;

            // Remove calculation methods from original class
            var newTypeDecl = typeDeclaration.RemoveNodes(calculationMethods, SyntaxRemoveOptions.KeepNoTrivia);

            // Create new partial class with calculation methods
            var partialClass = SyntaxFactory.ClassDeclaration(typeDeclaration.Identifier)
                .WithModifiers(typeDeclaration.Modifiers.Add(SyntaxFactory.Token(SyntaxKind.PartialKeyword)))
                .WithMembers(SyntaxFactory.List<MemberDeclarationSyntax>(calculationMethods))
                .WithLeadingTrivia(typeDeclaration.GetLeadingTrivia())
                .WithTrailingTrivia(typeDeclaration.GetTrailingTrivia());

            // Insert new partial class after the original
            var newRoot = root.ReplaceNode(typeDeclaration, new[] { newTypeDecl, partialClass });

            return document.WithSyntaxRoot(newRoot);
        }

        private static async Task<Document> AddRefactoringComment(
    Document document,
    ClassDeclarationSyntax typeDeclaration,
    CancellationToken cancellationToken)
        {
            // Create a new comment line
            var commentText = SyntaxFactory.Comment("// TODO: Class violates SRP. Must refactor/extract interface.");

            // Add the comment before the class declaration, preserving leading trivia
            var leadingTrivia = typeDeclaration.GetLeadingTrivia();
            var newTrivia = SyntaxFactory.TriviaList(leadingTrivia.Concat(new[] { commentText, SyntaxFactory.CarriageReturnLineFeed }));

            var newDeclaration = typeDeclaration.WithLeadingTrivia(newTrivia);

            var root = await document.GetSyntaxRootAsync(cancellationToken).ConfigureAwait(false);
            var newRoot = root.ReplaceNode(typeDeclaration, newDeclaration);

            return document.WithSyntaxRoot(newRoot);
        }
    }
}
