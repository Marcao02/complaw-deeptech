package miniL4.typechecker

import miniL4.{TSet}
import miniL4.ast.Datatype
import miniL4.typechecker.stdlibTyping.stdDataTypes.bottomDType

abstract sealed class FnType {
  def rangeTypeOnArgTypes(argtypes: Iterable[Datatype]) : Datatype = {
    // todo: this'll need to be overloaded
    bottomDType
  }
}

abstract sealed class NonoverloadedFnType extends FnType {}

case class SimpleFnType(parts:Seq[Datatype]) extends NonoverloadedFnType {
  assert(parts.nonEmpty)
  val ran : Datatype = this.parts.last
  val dom : Seq[Datatype] = this.parts.view(0,this.parts.length - 2)

}

case class OverloadedFnType(parts: Set[SimpleFnType]) extends FnType {
  assert(parts.nonEmpty)
//  val range_memo : Map[Seq[Sort], Option[Sort]] = Map.empty
}

abstract sealed class ParametricSimpleFnType extends NonoverloadedFnType {}

case class SFTForEachDatatypeIn(dtset:TSet[Datatype], fntype: Datatype => NonoverloadedFnType) extends NonoverloadedFnType {}
//case class SFTForEachUnitsTypeIn(dtset:TSet[Sort], fntype: UnitsType => NonoverloadedFnType) extends NonoverloadedFnType {}

