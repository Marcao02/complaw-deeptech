package miniL4.ast

import miniL4.Name

abstract sealed class Datatype(loc:Loc) extends ASTNode(loc) {}
case class AtomicDatatype(name:Name, loc:Loc = NoLoc) extends Datatype(loc)
case class DatatypeOpApp(name:Name, args:Seq[Datatype], loc:Loc = NoLoc) extends Datatype(loc)

//abstract sealed class UnitsType {}