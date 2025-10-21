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
        // Define the resource titles (assuming CodeFixResources.RemoveExceptionTitle and ReplaceExceptionTitle exist)
        // If your resources are named CodeFixResources, use that namespace/class name.
        private static readonly string RemoveTitle = CodeFixResources.RemoveExceptionTitle;
        private static readonly string ReplaceTitle = CodeFixResources.ReplaceExceptionTitle;
        // 1. Tell Roslyn which diagnostics (rules) this fix provider can handle.
        public sealed override ImmutableArray<string> FixableDiagnosticIds
        {
            // NOTE: Assumes SolidAnalyzer is visible here. If not, use the full path or alias.
            get { return ImmutableArray.Create(SolidAnalyzersNuget.SolidAnalyzer.lspDiagnosticId); }
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
    }
}
