package miniL4.typechecker

import indy.type_abbrevs._
import indy.util._
import miniL4.ast.{Datatype, Term}
import miniL4.interpreter.RTData.RTData

/*
This seems like a heavy approach, effectively equivalent to having one type for closed datatypes and another for datatypes
with free variables. But, I think it's better than introducing variables into Datatype.
 */

abstract sealed class DatatypePattern

case class DatatypeMatchVar(name:Name) extends DatatypePattern
case class DatatypeOpAppPattern(op:Name, args:List[DatatypePattern]) extends DatatypePattern
case class FixedDatatype(datatype:Datatype) extends DatatypePattern

case class ConstantMatchVar(name:Name, datatype:Name)
case class DependentDatatypeOpAppPattern(op: Name, dataargs: List[ConstantMatchVar], args: List[DatatypePattern] = List.empty) extends DatatypePattern {
  override def toString: String = {
    if(args.isEmpty)
      op.toString() + "(" + toStringJoin(dataargs,", ") + ")"
    else
      op.toString() + "(" + toStringJoin(dataargs,", ") + "; " + toStringJoin(args,", ") + ")"
  }
}

case class DatatypePatSubst(forDatatypes: TMap[Name,Datatype], forData: TMap[Name,RTData])
object DatatypePatSubst {
  val empty = DatatypePatSubst(Map.empty[Name,Datatype], Map.empty[Name,RTData])
}
