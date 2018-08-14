package miniL4.examples

import miniL4.ast.astutil._
import miniL4.ast.time.{NoTimeConstraint, TimeConstraint}
import miniL4.ast.{SituationDef, _}
import miniL4.interpreter.L4Event
import miniL4.typechecker.stdlibTyping.stdDataTypes.{realDType,posRealDType}

object bank extends TestExample  {
  private val teller = List('Teller)
  private val customer = List('Customer)

  private val start_CashOnHand = 20.0
  private val start_AccountBalance = 1000.0
  private val MIN_WITHDRAW = 300.0

  val contract : Contract = Contract(List(
    StateVarDef('customerCashOnHand, realDType, Some(RealLit(start_CashOnHand))),
    StateVarDef('customerAccountBalance, realDType, Some(RealLit(start_AccountBalance))),
    StateVarDef('totalAccountChange, realDType, Some(RealLit(0))),

    SituationDef('AtCounter, List(
      ExternalEventRule('Withdraw, customer, NoTimeConstraint(NoLoc), None, List(hp2rp('amount)),
        Some(and(leq(hp2rp('amount), 'customerAccountBalance), geq(hp2rp('amount), MIN_WITHDRAW))
      )),
      ExternalEventRule('Deposit, customer, NoTimeConstraint(NoLoc), None, List(hp2rp('amount)),
        Some(leq(hp2rp('amount), 'customerCashOnHand))),
    )),

    EventHandlerDef('Withdraw, 'AtCounter, List(
      StateVarAssign('customerAccountBalance, minus('customerAccountBalance,'amount)),
      StateVarAssign('customerCashOnHand, plus('customerCashOnHand,'amount)),
    ), List(('amount, posRealDType)), List(geq('customerAccountBalance,'amount))),

    EventHandlerDef('Deposit, 'AtCounter, List(
      StateVarAssign('customerCashOnHand, minus('customerCashOnHand,'amount)),
      StateVarAssign('customerAccountBalance, plus('customerAccountBalance,'amount)),
    ), List(('amount, posRealDType)), List(geq('customerCashOnHand,'amount)))

  ))

  val traces = List(
    List(
      L4Event('Withdraw, 'Customer, 0, Map('amount -> 400.0)),
      L4Event('Deposit, 'Customer, 0, Map('amount -> 400.0))
    )
  )

  val exceptionTraces = List(
    List(
      L4Event('Withdraw, 'Customer, 0, Map('amount -> (MIN_WITHDRAW - 1)))
    ),
    List(
      L4Event('Withdraw, 'Customer, 0, Map('amount -> (1 + start_AccountBalance)))
    ),
    List(
      L4Event('Deposit, 'Customer, 0, Map('amount -> (1 + start_CashOnHand)))
    )
  )

}


//object TestRunner extends App {
//  println(ContractLinking.link(meng_buy_booze.contract))
//}