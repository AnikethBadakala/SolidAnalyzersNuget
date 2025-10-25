using Microsoft.CodeAnalysis.Testing;
using Microsoft.CodeAnalysis;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using System.Threading.Tasks;
using VerifyCS = SolidAnalyzersNuget.Test.CSharpCodeFixVerifier<
    SolidAnalyzersNuget.SolidAnalyzer,
    SolidAnalyzersNuget.CodeFixes.SolidAnalyzersNugetCodeFixProvider>;

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


        // Inside SolidAnalyzerUnitTests.cs
        [TestMethod]
        public async Task SrpViolation_ShouldTriggerDiagnostic()
        {
            var testCode = @"
using System;
using System.Collections.Generic;

public interface IEmployeeRepository { }
public interface ILogger { }
public class Employee { }

class GodClass 
{
    public List<Employee> GetAllActive() { return new List<Employee>(); }
    public IEmployeeRepository Repository { get; set; }

    // Business Logic/Calculations
    public decimal CalculateGrossPay(Employee emp) { return 0m; }
    public decimal CalculateTaxes(decimal gross) { return 0m; }
    public void ApplyPromotion(Employee emp, string title) { }
    public TimeSpan GetTimeInService(Employee emp) { return TimeSpan.Zero; }

    // UI/Presentation
    public string FormatForDisplay(Employee emp) { return string.Empty; }
    public string UiLabel { get; set; }
    
    public int SimpleField = 0; 
    public GodClass() { } 
}";

    var expected = new DiagnosticResult("DiagnosticId", DiagnosticSeverity.Warning)
        .WithLocation(line: 1, column: 1)
        .WithMessage("Your message here");

    await VerifyCS.VerifyAnalyzerAsync(testCode, expected);
        }
    }
}
