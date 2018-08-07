package miniL4.ast

import miniL4.{Name, mapToStringJoin}
import miniL4.ast.time.{NoTimeConstraint, TimeConstraint, TimeTrigger}

abstract sealed class EventRule(
  val eventDefName: Name,
  val enabledGuard: Option[Term],
  val ruleparamNames: Seq[Name],
//  val paramSetter: Seq[Term], // don't allow it for miniL4
  val loc:Loc = NoLoc) extends ASTNode(loc) {
  def minimalEventRuleToString() : String
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

  def paramSetterMap(): Map[Name,Term] = this.ruleparamNames.view.zip(this.paramSetter).toMap
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