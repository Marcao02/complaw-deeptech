package miniL4.typechecker

import miniL4.TSet
import miniL4.ast.{Datatype}

abstract sealed class SubtypingDec {
  val pairs : Set[(Datatype,Datatype)]
}
//abstract sealed class SubtypePair extends SubtypingDec {}

case class SimpleSubtypePair(left:Datatype, right:Datatype) extends SubtypingDec {
  val pairs = Set((left, right))
}
case class SimpleSubtypeChain(decs:Iterable[Datatype]) extends SubtypingDec {
  val pairs = {
    val decs_indexed = decs.toIndexedSeq
    (for( i <- decs_indexed.indices if i < decs_indexed.length - 1)
        yield (decs_indexed(i), decs_indexed(i + 1))).toSet
  }
}


abstract sealed class ParametricSubtypingDec extends SubtypingDec {}
case class PSDForEachDatatypeIn(dtset:Set[Datatype], fntype: Datatype => SubtypingDec) extends ParametricSubtypingDec {
  val pairs = this.dtset.flatMap(x => fntype(x).pairs)
}
//case class PSDForEachUnitsTypeIn(dtset:TSet[Datatype], fntype: UnitsType => SubtypingDec) extends ParametricSubtypingDec {}


