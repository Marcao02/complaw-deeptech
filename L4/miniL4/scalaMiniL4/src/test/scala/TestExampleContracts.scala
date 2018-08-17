import miniL4.EvalError
import miniL4.analysis.checks
import miniL4.ast.ContractLinking
import miniL4.examples.{TestExample, bank, following_situation_alternative, meng_buy_booze, typechecking_test}
import miniL4.interpreter.{Trace, evalL4}
import miniL4.typechecker.typechecker
import org.scalatest.FunSuite

object TestExampleContracts {
  val toTest : Seq[(String,TestExample)] = List(
    ("bank", bank),
    ("booze", meng_buy_booze),
    ("typechecking_only", typechecking_test)
//    ("following_situation_alternative", following_situation_alternative)

  )
}
class TestExampleContracts extends FunSuite {

  for ((name, example) <- TestExampleContracts.toTest) {
    val (contract,traces,exceptionTraces) = (example.contract, example.traces, example.exceptionTraces)

    val linking = ContractLinking.link(contract)

    test(s"no unresolved names in example '$name'") {
      checks.assertNamesResolve(contract, linking)
    }

    test(s"typecheck '$name'") {
      val proofobligs = typechecker.typecheckContract(linking)
      assert(proofobligs.size == 0)
    }

    for (trace <- traces) {
      test(s"run trace of '$name': " + Trace.minimalTraceString(trace)) {
        evalL4.evalTrace(trace, linking)
      }
    }

    for (trace <- exceptionTraces) {
      test(s"run errorful trace of '$name': " + Trace.traceStringWithEventParamsNoNamesNoTime(trace)) {
        try {
          evalL4.evalTrace(trace, linking)
          fail("Should have triggered an L4TraceException")
        } catch {
          case e:EvalError => () // all good
          case e:Throwable => fail(s"Should have triggered an L4TraceException, but got this instead: ${e}" )
        }
      }
    }
  }

}