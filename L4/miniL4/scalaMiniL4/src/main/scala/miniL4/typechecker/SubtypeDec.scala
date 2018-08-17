package miniL4.typechecker

import indy.type_abbrevs._
import miniL4.ast.{Datatype}

abstract sealed class SubtypingDec {
  val pairs : TSet[(Datatype,Datatype)]
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


//abstract sealed class ParametricSubtypingDec extends SubtypingDec {}
//case class PSDForEachDatatypeIn(dtset:TSet[Datatype], fntype: Datatype => SubtypingDec) extends ParametricSubtypingDec {
//  val pairs = this.dtset.flatMap(x => fntype(x).pairs)
//}
//case class PSDForEachUnitsTypeIn(dtset:TSet[Datatype], fntype: UnitsType => SubtypingDec) extends ParametricSubtypingDec {}


