package miniL4.typechecker

import miniL4.{Name, TMap}
import miniL4.ast.{Datatype, Term}
import miniL4.interpreter.RTData

/*
This seems like a heavy approach, effectively equivalent to having one type for closed datatypes and another for datatypes
with free variables. But, I think it's better than introducing variables into Datatype.
 */

abstract sealed class DatatypePattern

case class DatatypeMatchVar(name:Name) extends DatatypePattern
case class DatatypeOpAppPattern(op:Name, args:List[DatatypePattern]) extends DatatypePattern
case class FixedDatatype(datatype:Datatype) extends DatatypePattern

case class ConstantMatchVar(name:Name, datatype:Name)
case class DependentDatatypeOpAppPattern(op:Name, args:List[DatatypePattern], dataargs:List[ConstantMatchVar]) extends DatatypePattern

case class DatatypePatSubst(forDatatypes: TMap[Name,Datatype], forData: TMap[Name,RTData])
object DatatypePatSubst {
  val empty = DatatypePatSubst(Map.empty[Name,Datatype], Map.empty[Name,RTData])
}
