import miniL4.analysis.checks
import miniL4.ast.ContractLinking
import miniL4.examples.meng_buy_booze.{meng_buy_booze_contract, traces}
import miniL4.interpreter.{Trace, evalL4}
import org.scalatest.FunSuite

class BoozeToyExampleTests extends FunSuite {
  test("no unresolved names in meng_buy_booze example") {
    val clink = ContractLinking.link(meng_buy_booze_contract)
    checks.assertNamesResolve(meng_buy_booze_contract, clink)
  }

  for (trace <- traces) {
    test(Trace.minimalTraceString(trace)) {
      val clink = ContractLinking.link(meng_buy_booze_contract)
      evalL4.evalTrace(trace, clink)
    }
  }
}


  //     use the try catch later for a negative example
//  test("unresolved names in example_with_unresolved_name example") {
//    val link = ContractLinking.link(contract)
//    try {
//      checks.assertNamesResolve(contract, link)
//      assert(false)
//    } catch {
//      case e: AssertionError => {
//        println(s"Got assertion error that says ${e.getMessage}")
//      }
//    }
//  }
//}

