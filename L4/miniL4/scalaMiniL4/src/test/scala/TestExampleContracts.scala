import miniL4.analysis.checks
import miniL4.ast.ContractLinking
import miniL4.examples.bank.{bank_contract, traces => bank_traces}
import miniL4.examples.meng_buy_booze.{meng_buy_booze_contract => booze_contract, traces => booze_traces}
import miniL4.interpreter.{Trace, evalL4}
import miniL4.typechecker.typechecker
import org.scalatest.FunSuite

object TestExampleContracts {
  val toTest = List(
    ("bank", bank_contract, bank_traces),
    ("booze", booze_contract, booze_traces)
  )
}
class TestExampleContracts extends FunSuite {

  for ((name, contract,traces) <- TestExampleContracts.toTest) {

    val linking = ContractLinking.link(contract)

    test(s"no unresolved names in example '$name'") {
      checks.assertNamesResolve(contract, linking)
    }

    test(s"typecheck '$name'") {
      val proofobligs = typechecker.typecheckContract(linking)
      assert(proofobligs.size == 0)
    }

    for (trace <- traces) {
      test(s"'$name': " + Trace.minimalTraceString(trace)) {
        evalL4.evalTrace(trace, linking)
      }
    }
  }

}