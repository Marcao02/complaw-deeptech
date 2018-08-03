package ast

case class Contract(decs: Seq[ToplevelNode], loc: Loc = NoLoc) extends ASTNode(loc) {}