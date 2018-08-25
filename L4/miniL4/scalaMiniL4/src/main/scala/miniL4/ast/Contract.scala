package miniL4.ast

import indy.srcLocation.{Loc, NoLoc}

case class Contract(decs: Seq[ToplevelNode], loc: Loc = NoLoc) extends ASTNode(loc) {}