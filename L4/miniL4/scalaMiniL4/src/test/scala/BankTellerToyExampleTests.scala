import miniL4.analysis.checks
import miniL4.ast.ContractLinking
import miniL4.examples.bank.{bank_contract, traces}
import miniL4.interpreter.{Trace, evalL4}
import org.scalatest.FunSuite

class BankTellerToyExampleTests extends FunSuite {
  test("no unresolved names in bank contract example") {
    val clink = ContractLinking.link(bank_contract)
    checks.assertNamesResolve(bank_contract, clink)
  }

  for (trace <- traces) {
    test(Trace.minimalTraceString(trace)) {
      val clink = ContractLinking.link(bank_contract)
      evalL4.evalTrace(trace, clink)
    }
  }
}