package miniL4.ast

import miniL4.{Name, TMap}
import Statement.Block
import miniL4.interpreter.RTData

abstract sealed class ToplevelNode(loc:Loc) extends ASTNode(loc) {}

case class SituationDef(
   name: Name,
   eventRules: Seq[EventRule],
   preconditions: Seq[Term] = List(),
   loc: Loc = NoLoc) extends ToplevelNode(loc) {
}

// TODO: should I add roles parameter? or fine to leave it out in miniL4
case class EventHandlerDef(
                            eventName: Name,
                            destSit: Name,
                            stateTransform: Block = List(),
                            paramsAndDatatypes: Seq[(Name,Datatype)] = List(),
                            preconditions: Seq[Term] = List(),
                            loc: Loc = NoLoc) extends ToplevelNode(loc) {
  val params = paramsAndDatatypes.map(_._1)

  // TODO use this on Trace
  def paramValSubstOk(subst:TMap[Name,RTData]) : Boolean = {
    this.params.forall((pname) => {
      subst.contains(pname)
    }) && subst.size == this.paramsAndDatatypes.size
  }
}

case class StateVarDef(
                        name: Name,
                        dtype: Datatype,
                        initVal: Option[Term] = None,
                        modifiers: Seq[Symbol] = List(),
                        loc: Loc = NoLoc) extends ToplevelNode(loc) {
}

case class RegisteredDatatypes( dtypes: Set[Datatype], loc:Loc = NoLoc) extends ToplevelNode(loc)
