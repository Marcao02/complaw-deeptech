package miniL4.ast

import miniL4.Name

abstract sealed class Datatype(loc:Loc) extends ASTNode(loc) {}
case class AtomicDatatype(name:Name, loc:Loc = NoLoc) extends Datatype(loc)
case class DatatypeOpApp(name:Name, args:Seq[Datatype], loc:Loc = NoLoc) extends Datatype(loc)

// Not in miniL4:
//case class DatatypeAbbrevRef(abbrevname: Name, loc:Loc = NoLoc) extends Datatype(loc) {
//  def deref(link:ContractLinking) : Datatype = {
//    link.datatypeAbbrevs.getOrElse(this.abbrevname, throw new Exception(s"DatatypeAbbrevRef $abbrevname doesn't resolve to anything. This means your ContractLinking object is invalid, and there's probably a bug in ContractLinking.link"))
//  }
//}

//abstract sealed class UnitsType {}