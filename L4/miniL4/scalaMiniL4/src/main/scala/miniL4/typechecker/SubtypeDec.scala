package miniL4.typechecker

import miniL4.TSet
import miniL4.ast.{Datatype}

abstract sealed class SubtypingDec {}
abstract sealed class SubtypePair extends SubtypingDec {}

case class SimpleSubtypePair(left:Datatype, right:Datatype) extends SubtypePair
case class SimpleSubtypeChain(decs:Iterable[Datatype]) extends SubtypingDec

abstract sealed class ParametricSubtypingDec extends SubtypingDec {}
case class PSDForEachDatatypeIn(dtset:TSet[Datatype], fntype: Datatype => SubtypingDec) extends ParametricSubtypingDec {}
//case class PSDForEachUnitsTypeIn(dtset:TSet[Datatype], fntype: UnitsType => SubtypingDec) extends ParametricSubtypingDec {}


