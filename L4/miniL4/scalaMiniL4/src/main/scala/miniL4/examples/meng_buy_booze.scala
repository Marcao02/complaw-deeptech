package miniL4.examples
import miniL4.ast.{ContractLinking, _}
import miniL4.ast.time.timeUtil.{after_m, within_m}


object meng_buy_booze {
  val seller = List('Seller)
  val buyer = List('Buyer)

  val contract : Contract = Contract(List(
    StateVarDef('buyerMoney, AtomicSort('Real), Some(RealLit(20))),
    StateVarDef('cashRegister, AtomicSort('Real), Some(RealLit(200))),

    EventHandlerDef('ShowID, 'AfterShowID),

    SituationDef('AfterShowID, List(
      ExternalEventRule('RefuseID, seller),
      ExternalEventRule('RefuseIntoxicatedBuyer, seller),
      ExternalEventRule('AcceptIDAndSobrietyOfBuyer, seller)
    )),

    SituationDef('AtCounter, List(
      ExternalEventRule('ShowID, buyer)
    )),



    EventHandlerDef('RefuseIntoxicatedBuyer, 'Fulfilled),

    EventHandlerDef('RefuseID, 'Fulfilled),

    EventHandlerDef('AcceptIDAndSobrietyOfBuyer, 'AfterAcceptIDAndSobrietyOfBuyer),

    SituationDef('AfterAcceptIDAndSobrietyOfBuyer, List(
      ExternalEventRule('PayForBooze, buyer)
    )),

    EventHandlerDef('PayForBooze, 'AfterPayForBooze, List(
//      LocalVarAssign('pointlessLocalVar, RealLit(9)),
      StateVarAssign('buyerMoney, FnApp('-, List(NiT('buyerMoney), RealLit(10)))),
      StateVarAssign('cashRegister, FnApp('+, List(NiT('cashRegister), RealLit(10))))
    )),

    SituationDef('AfterPayForBooze, List(
      ExternalEventRule('DeliverBooze, seller, within_m(10)),
      ExternalEventRule('RefundBuyer, seller, within_m(15)),
      InternalEventRule('Breach_Seller, after_m(15))
    )),

    EventHandlerDef('DeliverBooze, 'Fulfilled),
    EventHandlerDef('RefundBuyer, 'Fulfilled, List(
      StateVarAssign('buyerMoney, FnApp('+, List(NiT('buyerMoney), RealLit(10)))),
      StateVarAssign('cashRegister, FnApp('-, List(NiT('cashRegister), RealLit(10))))
    )),

    EventHandlerDef('Breach_Seller, 'Breached_Seller),
    SituationDef('Breached_Seller, List())
  ))

}

//object TestRunner extends App {
//  println(ContractLinking.link(meng_buy_booze.contract))
//}