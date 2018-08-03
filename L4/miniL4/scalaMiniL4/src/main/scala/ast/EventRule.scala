package ast

import ast.time.{NoTimeConstraint, TimeConstraint, TimeTrigger}

abstract sealed class EventRule(
  val eventDefName: Name,
  val entranceGuard: Option[Term],
  val ruleparamNames: Seq[Name],
  val paramSetter: Seq[Term],
  val loc:Loc = NoLoc) extends ASTNode(loc) {
}

case class InternalEventRule(
    override val eventDefName: Name,
    timeTrigger: TimeTrigger,
    override val entranceGuard: Option[Term] = None,
    override val ruleparamNames: Seq[Name] = List(),
    override val paramSetter: Seq[Term] = List(),
    override val loc:Loc = NoLoc) extends EventRule(eventDefName, entranceGuard, ruleparamNames, paramSetter, loc)

case class ExternalEventRule(
    override val eventDefName: Name,
    roleIds: Seq[Name],
    timeConstraint: TimeConstraint = NoTimeConstraint(NoLoc),
    override val entranceGuard: Option[Term] = None,
    override val ruleparamNames: Seq[Name] = List(),
    override val paramSetter: Seq[Term] = List(),
    paramConstraint: Option[Term] = None,
    override val loc:Loc = NoLoc) extends EventRule(eventDefName, entranceGuard, ruleparamNames, paramSetter, loc)