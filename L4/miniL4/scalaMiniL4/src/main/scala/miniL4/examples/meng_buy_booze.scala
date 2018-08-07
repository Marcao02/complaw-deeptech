package miniL4.examples

import miniL4.ast.astutil.{minus, plus, geq}
import miniL4.ast.time.timeUtil.{after_m, within_m}
import miniL4.ast.{SituationDef, _}
import miniL4.interpreter.L4Event

object meng_buy_booze {
  val seller = List('Seller)
  val buyer = List('Buyer)

  val meng_buy_booze_contract : Contract = Contract(List(
    StateVarDef('buyerMoney, AtomicSort('Real), Some(RealLit(20))),
    StateVarDef('cashRegister, AtomicSort('Real), Some(RealLit(200))),

    SituationDef('AtCounter, List(
      ExternalEventRule('ShowID, buyer)
    )),

    EventHandlerDef('ShowID, 'AfterShowID),

    SituationDef('AfterShowID, List(
      ExternalEventRule('RefuseID, seller),
      ExternalEventRule('RefuseIntoxicatedBuyer, seller),
      ExternalEventRule('AcceptIDAndSobrietyOfBuyer, seller)
    )),

    EventHandlerDef('RefuseIntoxicatedBuyer, 'Fulfilled),

    EventHandlerDef('RefuseID, 'Fulfilled),

    EventHandlerDef('AcceptIDAndSobrietyOfBuyer, 'AfterAcceptIDAndSobrietyOfBuyer),

    SituationDef('AfterAcceptIDAndSobrietyOfBuyer, List(
      ExternalEventRule('PayForBooze, buyer)
    )),

    EventHandlerDef('PayForBooze, 'AfterPayForBooze, List(
//      LocalVarAssign('pointlessLocalVar, RealLit(9)),
      StateVarAssign('buyerMoney, minus('buyerMoney,10)),
      StateVarAssign('cashRegister, plus('cashRegister,10))
    ), List(), List(geq('buyerMoney,10)) ),

    SituationDef('AfterPayForBooze, List(
      ExternalEventRule('DeliverBooze, seller, within_m(10)),
      ExternalEventRule('RefundBuyer, seller, within_m(15)),
      InternalEventRule('Breach_Seller, after_m(15))
    )),

    EventHandlerDef('DeliverBooze, 'Fulfilled),
    EventHandlerDef('RefundBuyer, 'Fulfilled, List(
      StateVarAssign('buyerMoney, plus('buyerMoney,10)),
      StateVarAssign('cashRegister, minus('cashRegister,10)
    ))),

    EventHandlerDef('Breach_Seller, 'Breached_Seller),
    SituationDef('Breached_Seller, List()),

    SituationDef('Fulfilled, List())
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
      L4Event('DeliverBooze, 'Seller),
    ),

    List(
      L4Event('ShowID, 'Buyer),
      L4Event('AcceptIDAndSobrietyOfBuyer, 'Seller),
      L4Event('PayForBooze, 'Buyer),
      L4Event('RefundBuyer, 'Seller),
    )
  )

}

//object TestRunner extends App {
//  println(ContractLinking.link(meng_buy_booze.contract))
//}