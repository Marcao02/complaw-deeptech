package miniL4.examples

import miniL4.ast.astutil.{minus, plus, geq}
import miniL4.ast.time.timeUtil.{after_m, within_m}
import miniL4.ast._
import miniL4.interpreter.L4Event
import miniL4.typechecker.stdlibTyping.stdDataTypes.{realDType,posRealDType,boolDType}
import miniL4.examples.abbrevs._

object meng_buy_booze extends TestExample {
  val seller = List('Seller)
  val buyer = List('Buyer)

  val contract : Contract = Contract(List(
    svd('buyerMoney, realDType, Some(RealLit(20))),
    svd('cashRegister, realDType, Some(RealLit(200))),

    sd('AtCounter, List(
      eer('ShowID, buyer)
    )),

    ehd('ShowID, 'AfterShowID),

    sd('AfterShowID, List(
      eer('RefuseID, seller),
      eer('RefuseIntoxicatedBuyer, seller),
      eer('AcceptIDAndSobrietyOfBuyer, seller)
    )),

    ehd('RefuseIntoxicatedBuyer, 'Fulfilled),

    ehd('RefuseID, 'Fulfilled),

    ehd('AcceptIDAndSobrietyOfBuyer, 'AfterAcceptIDAndSobrietyOfBuyer),

    sd('AfterAcceptIDAndSobrietyOfBuyer, List(
      eer('PayForBooze, buyer)
    )),

    ehd('PayForBooze, 'AfterPayForBooze, List(
//      LocalVarAssign('pointlessLocalVar, RealLit(9)),
      sva('buyerMoney, minus('buyerMoney,10)),
      sva('cashRegister, plus('cashRegister,10))
    ), List(), List(geq('buyerMoney,10)) ),

    sd('AfterPayForBooze, List(
      eer('DeliverBooze, seller, within_m(10)),
      eer('RefundBuyer, seller, within_m(15)),
      ier('Breach_Seller, after_m(15))
    )),

    ehd('DeliverBooze, 'Fulfilled),
    ehd('RefundBuyer, 'Fulfilled, List(
      sva('buyerMoney, plus('buyerMoney,10)),
      sva('cashRegister, minus('cashRegister,10)
    ))),

    ehd('Breach_Seller, 'Breached_Seller),
    sd('Breached_Seller, List()),

    sd('Fulfilled, List())
  ))

  val traces = List(
    List(
      L4Event('ShowID, 'Buyer),
      L4Event('RefuseID, 'Seller)
    ),

    List(
      L4Event('ShowID, 'Buyer),
      L4Event('AcceptIDAndSobrietyOfBuyer, 'Seller),
      L4Event('PayForBooze, 'Buyer),
      L4Event('DeliverBooze, 'Seller)
    ),

    List(
      L4Event('ShowID, 'Buyer),
      L4Event('AcceptIDAndSobrietyOfBuyer, 'Seller),
      L4Event('PayForBooze, 'Buyer),
      L4Event('RefundBuyer, 'Seller)
    )
  )

  val exceptionTraces = List()

}

//object TestRunner extends App {
//  println(ContractLinking.link(meng_buy_booze.contract))
//}