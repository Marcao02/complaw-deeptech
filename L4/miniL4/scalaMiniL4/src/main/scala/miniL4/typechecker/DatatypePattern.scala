package miniL4.typechecker

import miniL4.{Name, TMap}
import miniL4.ast.{Datatype, Term}

/*
This seems like a heavy approach, effectively equivalent to having one type for closed datatypes and another for datatypes
with free variables. But, I think it's better than introducing variables into Datatype.
 */

abstract sealed class DatatypePattern {}
abstract sealed class MatchVar(name:Name) extends DatatypePattern

case class ConstantMatchVar(name:Name, datatype:Name) extends MatchVar(name) {}
case class DatatypeMatchVar(name:Name) extends MatchVar(name) {}

case class DatatypeOpAppPattern(op:Name, args:List[DatatypePattern]) extends DatatypePattern {}
case class FixedDatatype(datatype:Datatype) extends DatatypePattern {}

object DatatypePattern {
  type DatatypePatSubst = TMap[Name, Any]
}