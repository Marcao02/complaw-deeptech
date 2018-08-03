package miniL4.ast

import miniL4.Name
import miniL4.ast.time.{NoTimeConstraint, TimeConstraint, TimeTrigger}

abstract sealed class EventRule(
  val eventDefName: Name,
  val enabledGuard: Option[Term],
  val ruleparamNames: Seq[Name],
//  val paramSetter: Seq[Term], // don't allow it for miniL4
  val loc:Loc = NoLoc) extends ASTNode(loc) {
}

case class InternalEventRule(
    override val eventDefName: Name,
    timeTrigger: TimeTrigger,
    override val enabledGuard: Option[Term] = None,
    override val ruleparamNames: Seq[Name] = List(),
    paramSetter: Seq[Term] = List(),
    override val loc:Loc = NoLoc) extends EventRule(eventDefName, enabledGuard, ruleparamNames, /*paramSetter,*/ loc)

case class ExternalEventRule(
    override val eventDefName: Name,
    roleIds: Seq[Name],
    timeConstraint: TimeConstraint = NoTimeConstraint(NoLoc),
    override val enabledGuard: Option[Term] = None,
    override val ruleparamNames: Seq[Name] = List(),
//    override val paramSetter: Seq[Term] = List(),  // don't allow it for miniL4. will convert it to paramConstraint in full L4
    paramConstraint: Option[Term] = None,
    override val loc:Loc = NoLoc) extends EventRule(eventDefName, enabledGuard, ruleparamNames, /*paramSetter,*/ loc)