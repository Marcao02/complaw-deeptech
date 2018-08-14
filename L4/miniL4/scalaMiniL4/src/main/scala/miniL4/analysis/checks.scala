package miniL4.analysis

import miniL4.ast._
//import org.scalatest.Assertions._

object checks {
  def assertNamesResolve(cprog:Contract, link:ContractLinking) : Unit = {
    def f(x:ASTNode) : Unit = {
      x match {
        case StateVarAssign(name, _, _) => assert( link.stateVarDefs.contains(name), s"StateVarDef for ${name} not found.")
        case EventHandlerDef(_, destSit, _, _, _, _) => assert( link.eventHandlerDefs.contains(destSit), s"SituationDef for ${destSit} not found.")
        case er:EventRule => assert( link.eventHandlerDefs.contains(er.eventDefName), s"EventHandlerDef for ${er.eventDefName} not found.")
        case nit:NiT => assert( nit.defn(link) != NoBinder, s"Name-in-term ${nit.name} is not bound.")
        case _ => ()
      }
    }
    astutil.forEachNodeInContract(cprog,f)
  }
}
