package miniL4.examples

import miniL4.ast._
import miniL4.ast.astutil._
import miniL4.typechecker.stdlibTyping.stdDataTypes._

object typechecking_test extends TestExample {

  private val me = List('Me)

  val contract = Contract(List(
    StateVarDef('a_real, realDType, Some(RealLit(20))),
    StateVarDef('a_posreal, posRealDType, Some(RealLit(200))),
    StateVarDef('a_nonnegreal, nonnegRealDType, Some(RealLit(0))),
    StateVarDef('a_bool, boolDType, Some(BoolLit(false))),
    StateVarDef('a_num_in_03, DependentDatatypeOpApp('OCInterval,List(0.0,3.0)), Some(RealLit(2.0))),

    SituationDef('TheSit, List(
      ExternalEventRule('TheAct, me)
    )),

    EventHandlerDef('TheAct, 'TheSit, List(
      IfElse( not(FnApp('>=, List(NiT('a_real), NiT('a_posreal)))),
        List(
          StateVarAssign('a_nonnegreal, plus('a_posreal, 'a_posreal)),
          StateVarAssign('a_nonnegreal, NiT('a_num_in_03))
        ),
        List(
          // would cause failure:
          AssertTypeError(StateVarAssign('a_posreal, plus('a_real, 'a_real)))
        )

      )
    ))
  ))

  val traces = List()
  val exceptionTraces = List()

}
