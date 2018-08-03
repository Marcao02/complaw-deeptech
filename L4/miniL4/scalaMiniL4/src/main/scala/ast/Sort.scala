package ast

abstract sealed class Sort(loc:Loc) extends ASTNode(loc) {}
  case class AtomicSort(name:Name, loc:Loc = NoLoc) extends Sort(loc)
  case class SortOpApp(name:Name, args:Seq[Sort], loc:Loc = NoLoc) extends Sort(loc)
