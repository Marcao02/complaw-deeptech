package miniL4.examples

import miniL4.ast._
import miniL4.ast.astutil._
import miniL4.ast.time.timeUtil.{after_m, within_m}
import miniL4.typechecker.stdlibTyping.stdDataTypes._

object typechecking_test extends TestExample {

  private val me = List('Me)

  val contract = Contract(List(
    StateVarDef('real, realDType, Some(RealLit(20))),
    StateVarDef('posreal, posRealDType, Some(RealLit(200))),
    StateVarDef('nonnegreal, nonnegRealDType, Some(RealLit(0))),
    StateVarDef('bool, boolDType, Some(BoolLit(false))),

    SituationDef('TheSit, List(
      ExternalEventRule('TheAct, me)
    )),

    EventHandlerDef('TheAct, 'TheSit, List(
      IfElse( not(FnApp('>=, List(NiT('real), NiT('posreal)))),
        List(
          StateVarAssign('nonnegreal, plus('posreal, 'posreal))

        ),
        // would cause failure:
        List(
          AssertTypeError(StateVarAssign('posreal, plus('real, 'real)))
        )
      )
    ))
  ))

  val traces = List()
  val exceptionTraces = List()

}
