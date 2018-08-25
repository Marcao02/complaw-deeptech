package miniL4.ast.time

import indy.srcLocation.{Loc, NoLoc}
import miniL4.ast.{ASTNode, TimeDeltaLit}

abstract sealed class TimeConstraint(loc:Loc) extends ASTNode(loc) {}

case class BeforeTimeDeltaFromStart(td:TimeDeltaLit,loc: Loc = NoLoc) extends TimeConstraint(loc)
case class WithinTimeDeltaFromStart(td:TimeDeltaLit,loc: Loc = NoLoc) extends TimeConstraint(loc)

case class BeforeTimeDeltaSplit(td:TimeDeltaLit,loc: Loc = NoLoc) extends TimeConstraint(loc)
case class WithinTimeDeltaSplit(td:TimeDeltaLit,loc: Loc = NoLoc) extends TimeConstraint(loc)

case class NoTimeConstraint(loc: Loc = NoLoc) extends TimeConstraint(loc)