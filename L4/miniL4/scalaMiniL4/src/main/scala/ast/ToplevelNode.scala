package ast

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
    loc: Loc = NoLoc) extends ToplevelNode(loc) {
}

case class StateVarDef(
    name: Name,
    sort: Sort,
    initVal: Option[Term] = None,
    modifiers: Seq[Symbol] = List(),
    loc: Loc = NoLoc) extends ToplevelNode(loc) {
}

