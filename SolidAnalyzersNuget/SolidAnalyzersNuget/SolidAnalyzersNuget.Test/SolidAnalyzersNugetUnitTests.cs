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
                .WithArguments("DoWork", "DerivedClass", "InvalidOperationException", "DoWork");

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
    public ILogger Logger { get; set; }
    public string UiLabel { get; set; }
    public int SimpleField = 0; 
    public GodClass() { } 

    // Business Logic/Calculations
    public decimal CalculateGrossPay(Employee emp) { return 0m; }
    public decimal CalculateTaxes(decimal gross) { return 0m; }
    public void ApplyPromotion(Employee emp, string title) { }
    public TimeSpan GetTimeInService(Employee emp) { return TimeSpan.Zero; }
    public void Promote(Employee emp) { }
    public void Demote(Employee emp) { }
    public void Transfer(Employee emp) { }
    public void Terminate(Employee emp) { }
    public void Rehire(Employee emp) { }
    public void Archive(Employee emp) { }
    public void Restore(Employee emp) { }
    public void Suspend(Employee emp) { }
    public void Reinstate(Employee emp) { }
    public void SetSalary(Employee emp, decimal salary) { }
    public void SetBonus(Employee emp, decimal bonus) { }
}";

    // Update to match actual diagnostic location and member count
    //test //dev
    var expected = VerifyCS.Diagnostic(SolidAnalyzer.srpDiagnosticId)
        .WithSpan(9, 7, 9, 15) // Line and column for 'GodClass'
        .WithArguments("GodClass", 27, 5); // Type name, actual member count, threshold

    await VerifyCS.VerifyAnalyzerAsync(testCode, expected);
        }
    }
}
