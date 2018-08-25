package miniL4.ast.time

import indy.srcLocation.{Loc, NoLoc}
import miniL4.ast.{ASTNode, TimeDeltaLit}

abstract sealed class TimeTrigger(loc:Loc) extends ASTNode(loc) {}

case class AtTimeDelta(td:TimeDeltaLit, loc: Loc = NoLoc) extends TimeTrigger(loc)
case class AfterTimeDelta(td:TimeDeltaLit, loc: Loc = NoLoc) extends TimeTrigger(loc)

