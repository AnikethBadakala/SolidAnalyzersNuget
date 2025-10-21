using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Threading.Tasks;
using VerifyCS = SolidAnalyzersNuget.Test.CSharpCodeFixVerifier<
    SolidAnalyzersNuget.SolidAnalyzer,
    SolidAnalyzersNuget.SolidAnalyzersNugetCodeFixProvider>;

namespace SolidAnalyzersNuget.Test
{
    [TestClass]
    public class SolidAnalyzersNugetUnitTest
    {
        [TestMethod]
        public async Task LspViolation_ShouldTriggerDiagnostic()
        {
            // Sample C# code that violates LSP:
            var testCode = @"
using System;

class BaseClass
{
    public virtual void DoWork() { }
}

class DerivedClass : BaseClass
{
    public override void DoWork()
    {
        throw new InvalidOperationException();
    }
}";

            // The expected diagnostic location
            var expected = VerifyCS.Diagnostic(SolidAnalyzer.lspDiagnosticId)
                .WithSpan(13, 9, 13, 47) // Line, start column, end column of the 'throw' statement
                .WithArguments("DoWork", "InvalidOperationException");

            await VerifyCS.VerifyAnalyzerAsync(testCode, expected);
        }

        [TestMethod]
        public async Task Test_EmptyMethod_ShouldNotTriggerDiagnostic()
        {
            var testCode = @"
class Base
{
    public virtual void Foo() { }
}

class Derived : Base
{
    public override void Foo() { }
}";

            await VerifyCS.VerifyAnalyzerAsync(testCode);
        }

    }
}
