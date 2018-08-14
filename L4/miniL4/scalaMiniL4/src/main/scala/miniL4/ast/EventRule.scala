package miniL4.ast

import miniL4.{Name, TMap, mapToStringJoin}
import miniL4.ast.time.{NoTimeConstraint, TimeConstraint, TimeTrigger}

abstract sealed class EventRule(
  val eventDefName: Name,
  val enabledGuard: Option[Term],
  val ruleparamNames: Seq[Name],
//  val paramSetter: Seq[Term], // don't allow it for miniL4
  val loc:Loc = NoLoc) extends ASTNode(loc) {
  def minimalEventRuleToString() : String
  def eventHandler(linking:ContractLinking) : EventHandlerDef = linking.eventHandlerDefs(this.eventDefName)
  def paramTypePairs(linking:ContractLinking) : Seq[(Name,Datatype)] = this.eventHandler(linking).paramsAndDatatypes
  def getEventHandlerParamTypePair(paramname:Name, linking:ContractLinking) : (Name, Datatype) = {
    val ind = this.ruleparamNames.indexOf(paramname)
    this.eventHandler(linking).paramsAndDatatypes(ind)
  }
  def paramToType(linking:ContractLinking) : TMap[Name,Datatype] = {
    this.eventHandler(linking).paramsAndDatatypes.toMap
  }

}
object EventRule {
  def minimalEventRuleCollToString(ers:Iterable[EventRule]) : String = {
    mapToStringJoin[EventRule](ers, "\n", _.minimalEventRuleToString())
  }

}

case class InternalEventRule(
    override val eventDefName: Name,
    timeTrigger: TimeTrigger,
    override val enabledGuard: Option[Term] = None,
    override val ruleparamNames: Seq[Name] = List(),
    paramSetter: Seq[Term] = List(),
    override val loc:Loc = NoLoc) extends EventRule(eventDefName, enabledGuard, ruleparamNames, /*paramSetter,*/ loc) {
  def minimalEventRuleToString() : String = {
    this.eventDefName + "..."
  }
  lazy val paramSetterPairs : Seq[(Name,Term)] = this.ruleparamNames.view.zip(this.paramSetter)
  lazy val paramSetterMap : Map[Name,Term] = this.paramSetterPairs.toMap
}

case class ExternalEventRule(
    override val eventDefName: Name,
    roleIds: Seq[Name],
    timeConstraint: TimeConstraint = NoTimeConstraint(NoLoc),
    override val enabledGuard: Option[Term] = None,
    override val ruleparamNames: Seq[Name] = List(),
//    override val paramSetter: Seq[Term] = List(),  // don't allow it for miniL4. will convert it to paramConstraint in full L4
    paramConstraint: Option[Term] = None,
    override val loc:Loc = NoLoc) extends EventRule(eventDefName, enabledGuard, ruleparamNames, /*paramSetter,*/ loc) {
  def minimalEventRuleToString() : String = {
    (mapToStringJoin[Name](this.roleIds, " or ", _.toString()) + " may " + this.eventDefName + "...")
  }
}