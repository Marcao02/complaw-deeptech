package miniL4.ast.time

import miniL4.ast.{ASTNode, Loc, TimeDeltaLit, NoLoc}

abstract sealed class TimeTrigger(loc:Loc) extends ASTNode(loc) {}

case class AtTimeDelta(td:TimeDeltaLit, loc: Loc = NoLoc) extends TimeTrigger(loc)
case class AfterTimeDelta(td:TimeDeltaLit, loc: Loc = NoLoc) extends TimeTrigger(loc)

