package miniL4.ast

import indy.srcLocation.{Loc, NoLoc}
import miniL4.interpreter.RTData.RTData
import indy.type_abbrevs._
import indy.util._
import miniL4.typechecker.stdlibTyping.stdDataTypes.sBottom

object Datatype {
  def datatypeOpAppHelper(op:Name)(args:Any*) = {
    val args_wrapped = args.map {
      case _arg: Name => AtomicDatatype(_arg)
      case _arg: Datatype => _arg
    }
    DatatypeOpApp(op, args_wrapped.toList)
  }
}

abstract sealed class Datatype(loc:Loc) extends ASTNode(loc) {
  def isBottom : Boolean = this match {
    case AtomicDatatype(name,_) => name == sBottom
    case _ => false
  }
}
  case class AtomicDatatype(name:Name, loc:Loc = NoLoc) extends Datatype(loc) {
    override def toString: String = name.toString()

    override def equals(obj: scala.Any): Boolean = obj match {
      case AtomicDatatype(name2,_) => name == name2
      case _ => false
    }
  }
  case class DatatypeOpApp(name:Name, args:Seq[Datatype], loc:Loc = NoLoc) extends Datatype(loc) {
    override def toString: String = name.toString() + "(" + mapToStringJoin[Datatype](args, ",", x => x.toString()) + ")"
    override def equals(obj: scala.Any): Boolean = obj match {
      case DatatypeOpApp(name2,args2,_) => name == name2 && args == args2
      case _ => false
    }
  }
  case class DependentDatatypeOpApp(name: Name, dataargs: Seq[RTData], args: Seq[Datatype] = List.empty, loc: Loc = NoLoc) extends Datatype(loc) {
    override def toString: String = {
      if(args.isEmpty)
        name.toString() + "(" +
          mapToStringJoin[RTData](dataargs, ",", x => x.toString) + ")"
      else
        name.toString() + "(" +
          mapToStringJoin[RTData](dataargs, ",", x => x.toString) +
          mapToStringJoin[Datatype](args, ",", x => x.toString) + "; " + ")"
    }
    override def equals(obj: scala.Any): Boolean = obj match {
      case DatatypeOpApp(name2,args2,_) => name == name2 && args == args2
      case _ => false
    }
  }

// Not in miniL4:
//case class DatatypeAbbrevRef(abbrevname: Name, loc:Loc = NoLoc) extends Datatype(loc) {
//  def deref(link:ContractLinking) : Datatype = {
//    link.datatypeAbbrevs.getOrElse(this.abbrevname, throw new Exception(s"DatatypeAbbrevRef $abbrevname doesn't resolve to anything. This means your ContractLinking object is invalid, and there's probably a bug in ContractLinking.link"))
//  }
//}

//abstract sealed class UnitsType {}