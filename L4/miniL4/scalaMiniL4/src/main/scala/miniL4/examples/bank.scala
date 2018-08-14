package miniL4.examples

import miniL4.ast.astutil._
import miniL4.ast.time.{NoTimeConstraint, TimeConstraint}
import miniL4.ast.{SituationDef, _}
import miniL4.interpreter.L4Event

object bank extends TestExample  {
  private val teller = List('Teller)
  private val customer = List('Customer)

  val contract : Contract = Contract(List(
    StateVarDef('customerCashOnHand, AtomicDatatype('Real), Some(RealLit(20))),
    StateVarDef('customerAccountBalance, AtomicDatatype('Real), Some(RealLit(1000))),
    StateVarDef('totalAccountChange, AtomicDatatype('Real), Some(RealLit(0))),

    SituationDef('AtCounter, List(
      ExternalEventRule('Withdraw, customer, NoTimeConstraint(NoLoc), None, List('amount), Some(
        and(leq('amount, 'customerAccountBalance), geq('amount, 300))
      )),
      ExternalEventRule('Deposit, customer, NoTimeConstraint(NoLoc), None, List('amount), Some(leq('amount, 'customerCashOnHand))),
    )),

    EventHandlerDef('Withdraw, 'AtCounter, List(
      StateVarAssign('customerAccountBalance, minus('customerAccountBalance,'amount)),
      StateVarAssign('customerCashOnHand, plus('customerCashOnHand,'amount)),
    ), List(('amount,AtomicDatatype('Real))), List(geq('customerAccountBalance,'amount))),

    EventHandlerDef('Deposit, 'AtCounter, List(
      StateVarAssign('customerCashOnHand, minus('customerCashOnHand,'amount)),
      StateVarAssign('customerAccountBalance, plus('customerAccountBalance,'amount)),
    ), List(('amount,AtomicDatatype('Real))), List(geq('customerCashOnHand,'amount)))

  ))

  val traces = List(
    List(
      L4Event('Withdraw, 'Customer, 0, Map('amount -> 400.0)),
      L4Event('Deposit, 'Customer, 0, Map('amount -> 400.0))
    )
  )

  val exceptionTraces = List(
    List(
      L4Event('Withdraw, 'Customer, 0, Map('amount -> 299.0))
    )
  )

}


//object TestRunner extends App {
//  println(ContractLinking.link(meng_buy_booze.contract))
//}