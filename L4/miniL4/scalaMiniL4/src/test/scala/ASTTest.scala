import analysis.checks
import ast.ContractLinking
import examples.meng_buy_booze.contract
import org.scalatest.FunSuite

class ASTTest extends FunSuite {

  test("no unresolved names in meng_buy_booze example") {
    val link = ContractLinking.link(contract)
    checks.assertNamesResolve(contract, link)
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
}

