package miniL4.ast

import miniL4.{Block, Name}

abstract sealed class ToplevelNode(loc:Loc) extends ASTNode(loc) {}

case class SituationDef(
   name: Name,
   eventRules: Seq[EventRule],
   preconditions: Seq[Term] = List(),
   loc: Loc = NoLoc) extends ToplevelNode(loc) {
}

case class EventHandlerDef(
    eventName: Name,
    destSit: Name,
    stateTransform: Block = List(),
    paramAndSorts: Seq[(Name,Sort)] = List(),
    preconditions: Seq[Term] = List(),
    loc: Loc = NoLoc) extends ToplevelNode(loc) {
  val params = paramAndSorts.map(_._1)
}

case class StateVarDef(
    name: Name,
    sort: Sort,
    initVal: Option[Term] = None,
    modifiers: Seq[Symbol] = List(),
    loc: Loc = NoLoc) extends ToplevelNode(loc) {
}

