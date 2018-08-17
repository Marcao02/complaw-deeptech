package miniL4.examples

import miniL4.ast.astutil._
import miniL4.ast.time.{NoTimeConstraint, TimeConstraint}
import miniL4.ast._
import miniL4.interpreter.L4Event
import miniL4.typechecker.stdlibTyping.stdDataTypes.{realDType,posRealDType}

import miniL4.examples.abbrevs._

object bank extends TestExample  {
  private val teller = List('Teller)
  private val customer = List('Customer)

  private val start_CashOnHand = 20.0
  private val start_AccountBalance = 1000.0
  private val MIN_WITHDRAW = 300.0

  val contract : Contract = Contract(List(
    svd('customerCashOnHand, realDType, Some(RealLit(start_CashOnHand))),
    svd('customerAccountBalance, realDType, Some(RealLit(start_AccountBalance))),
    svd('totalAccountChange, realDType, Some(RealLit(0))),

    sd('AtCounter, List(
      eer('Withdraw, customer, NoTimeConstraint(), None, List(hp2rp('amount)),
        Some(and(leq(hp2rp('amount), 'customerAccountBalance), geq(hp2rp('amount), MIN_WITHDRAW))
      )),
      eer('Deposit, customer, NoTimeConstraint(), None, List(hp2rp('amount)),
        Some(leq(hp2rp('amount), 'customerCashOnHand)))
    )),

    ehd('Withdraw, 'AtCounter, List(
      sva('customerAccountBalance, minus('customerAccountBalance,'amount)),
      sva('customerCashOnHand, plus('customerCashOnHand,'amount))
    ), List(('amount, posRealDType)), List(geq('customerAccountBalance,'amount))),

    ehd('Deposit, 'AtCounter, List(
      sva('customerCashOnHand, minus('customerCashOnHand,'amount)),
      sva('customerAccountBalance, plus('customerAccountBalance,'amount))
    ), List(('amount, posRealDType)), List(geq('customerCashOnHand,'amount)))

  ))

  val traces = List(
    List(
      L4Event('Withdraw, 'Customer, 0, Map('amount -> d(400))),
      L4Event('Deposit, 'Customer, 0, Map('amount -> d(400)))
    )
  )

  val exceptionTraces = List(
    List(
      L4Event('Withdraw, 'Customer, 0, Map('amount -> d(MIN_WITHDRAW - 1)))
    ),
    List(
      L4Event('Withdraw, 'Customer, 0, Map('amount -> d(1 + start_AccountBalance)))
    ),
    List(
      L4Event('Deposit, 'Customer, 0, Map('amount -> d(1 + start_CashOnHand)))
    )
  )

}


//object TestRunner extends App {
//  println(ContractLinking.link(meng_buy_booze.contract))
//}