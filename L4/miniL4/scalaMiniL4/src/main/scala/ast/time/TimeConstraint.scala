package ast.time

import ast.{ASTNode, Loc, NoLoc, TimeDeltaLit}

abstract sealed class TimeConstraint(loc:Loc) extends ASTNode(loc) {}

case class BeforeTimeDeltaFromStart(td:TimeDeltaLit,loc: Loc = NoLoc) extends TimeConstraint(loc)
case class WithinTimeDeltaFromStart(td:TimeDeltaLit,loc: Loc = NoLoc) extends TimeConstraint(loc)
case class BeforeTimeDeltaSplit(td:TimeDeltaLit,loc: Loc = NoLoc) extends TimeConstraint(loc)
case class WithinTimeDeltaSplit(td:TimeDeltaLit,loc: Loc = NoLoc) extends TimeConstraint(loc)
case class NoTimeConstraint(loc: Loc = NoLoc) extends TimeConstraint(loc)