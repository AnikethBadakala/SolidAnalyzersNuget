using Microsoft.CodeAnalysis;
using Microsoft.CodeAnalysis.CodeActions;
using Microsoft.CodeAnalysis.CodeFixes;
using Microsoft.CodeAnalysis.CSharp;
using Microsoft.CodeAnalysis.CSharp.Syntax;
using Microsoft.CodeAnalysis.Rename;
using Microsoft.CodeAnalysis.Text;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Composition;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace SolidAnalyzersNuget
{
    [ExportCodeFixProvider(LanguageNames.CSharp, Name = nameof(SolidAnalyzersNugetCodeFixProvider)), Shared]
    public class SolidAnalyzersNugetCodeFixProvider : CodeFixProvider
    {
        // 1. Tell Roslyn which diagnostics (rules) this fix provider can handle.
        // It must match the ID defined in your analyzer: public const string lspDiagnosticId = "LSP001";
        // This property returns an immutable list of Diagnostic IDs this provider can fix.
        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            // Creates the immutable array containing only the LSP001 ID from your analyzer class.
            get { return ImmutableArray.Create(SolidAnalyzersNuget.SolidAnalyzer.lspDiagnosticId); }
        }

        // This method determines how to apply a fix to multiple occurrences of the same diagnostic.
        public sealed override FixAllProvider GetFixAllProvider()
        {
            // Recommended to return the default FixAll provider, which groups fixes efficiently.
            return WellKnownFixAllProviders.BatchFixer;
        }

        // 2. Register the fix action(s) for the detected diagnostic.
        // This is the core method where you inspect the problem and offer solutions (CodeActions).
        public sealed override async Task RegisterCodeFixesAsync(CodeFixContext context)
        {
            // Get the root node of the current document's syntax tree (the full source file).
            var root = await context.Document.GetSyntaxRootAsync(context.CancellationToken).ConfigureAwait(false);

            // Get the first (and usually only) diagnostic reported at the current cursor position.
            var diagnostic = context.Diagnostics.First();

            // Get the location (span) of the diagnostic in the source file.
            var diagnosticSpan = diagnostic.Location.SourceSpan;

            // Use the span to locate the specific C# syntax node causing the issue.
            // It starts at the diagnostic's location, finds the token, and then looks up the hierarchy 
            // to find the full 'ThrowStatementSyntax' node (e.g., 'throw new Exception();').
            var throwStatement = root.FindToken(diagnosticSpan.Start).Parent.AncestorsAndSelf().OfType<ThrowStatementSyntax>().First();

            // Register a code action to remove the violating throw statement.
            var action = CodeAction.Create(
                // The title displayed to the user in the Quick Actions menu (lightbulb).
                title: "Remove new exception (LSP violation)",
                // A function that, when executed, will create the fixed Document.
                createChangedDocument: c => RemoveThrowStatement(context.Document, root, throwStatement, c),
                // A unique key for grouping this fix with others if using a FixAll provider.
                equivalenceKey: nameof(SolidAnalyzersNugetCodeFixProvider)
            );

            // Officially register the CodeAction, making it available in the IDE.
            context.RegisterCodeFix(action, diagnostic);
        }

        // 3. The method that creates the new (fixed) document.
        private static Task<Document> RemoveThrowStatement(
            Document document, // The original document to fix.
            SyntaxNode root, // The root of the original document's syntax tree.
            ThrowStatementSyntax throwStatement, // The specific node to remove.
            CancellationToken cancellationToken) // Cancellation token for async operations.
        {
            // Remove the 'throwStatement' node from the syntax tree and return the new tree root.
            // SyntaxRemoveOptions.KeepNoTrivia ensures any leading/trailing whitespace or comments 
            // belonging only to the removed statement are also cleaned up.
            var newRoot = root.RemoveNode(throwStatement, SyntaxRemoveOptions.KeepNoTrivia);

            // Creates a new Document based on the original, but with the modified syntax tree.
            return Task.FromResult(document.WithSyntaxRoot(newRoot));
        }
    }
}
