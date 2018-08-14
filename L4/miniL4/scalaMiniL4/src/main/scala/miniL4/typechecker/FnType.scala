package miniL4.typechecker

import miniL4.{TSet, mapToStringJoin}
import miniL4.ast.{Datatype, DatatypeOpApp}
import miniL4.typechecker.stdlibTyping.stdDataTypes.bottomDType

abstract sealed class FnType {
  def rangeTypeOnArgTypes(argtypes: Seq[Datatype], graph:SubtypingGraph) : Datatype
}

abstract sealed class NonoverloadedFnType extends FnType {}

case class SimpleFnType(parts:Seq[Datatype]) extends NonoverloadedFnType {
  assert(parts.nonEmpty)
  val dom : Seq[Datatype] = this.parts.view(0,this.parts.length - 1)
  val ran : Datatype = this.parts.last
  assert(!ran.isBottom)

//  println(this)

  def rangeTypeOnArgTypes(argtypes: Seq[Datatype], graph:SubtypingGraph) : Datatype = {
    if(argtypes.length != this.dom.length) {
      println("arity issue:\n", argtypes.view.force, "\n", this.dom.view.force)
      bottomDType
    }
    else {
      for( i <- argtypes.indices ) {
        if (!graph.subtype(argtypes(i), this.dom(i))) {
//          println(s"${argtypes(i)} ⊆ ${this.dom(i)} is FALSE")
          return bottomDType
        }
      }
      this.ran
    }
  }

  override def toString: String = "(" + mapToStringJoin[Datatype](dom, delim = ",", x => x.toString) + ") => " + ran.toString()
//  override def equals(obj: scala.Any): Boolean = obj match {
//    case DatatypeOpApp(name2,args2,_) => name == name2 && args == args2
//    case _ => false
//  }
}

case class OverloadedFnType(parts: Set[SimpleFnType]) extends FnType {
  assert(parts.nonEmpty)
  //  val range_memo : Map[Seq[Sort], Option[Sort]] = Map.empty

  def rangeTypeOnArgTypes(argtypes: Seq[Datatype], graph: SubtypingGraph) : Datatype = {
    var in_range = List.empty[Datatype]
    for( sft <- parts ) {
      val result = sft.rangeTypeOnArgTypes(argtypes, graph)
      if( !result.isBottom )
        in_range = result::in_range
    }
    if(in_range.isEmpty) {
      println(s"${argtypes.toList} is not in the domain of a function of type:\n${parts}")
      return bottomDType
    }
    val intersection = graph.simplifyIntersection(in_range)
    assert(!intersection.isBottom)
    intersection
  }
}

abstract sealed class ParametricSimpleFnType extends NonoverloadedFnType {}

case class SFTForEachDatatypeIn(dtset:TSet[Datatype], fntype: Datatype => NonoverloadedFnType) extends NonoverloadedFnType {
  def rangeTypeOnArgTypes(argtypes: Seq[Datatype], graph:SubtypingGraph) : Datatype = bottomDType
}
//case class SFTForEachUnitsTypeIn(dtset:TSet[Sort], fntype: UnitsType => NonoverloadedFnType) extends NonoverloadedFnType {}

