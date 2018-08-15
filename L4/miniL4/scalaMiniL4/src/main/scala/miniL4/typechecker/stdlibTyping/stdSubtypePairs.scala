package miniL4.typechecker.stdlibTyping

import miniL4.{Name, TSet}
import miniL4.ast.{AtomicDatatype, Datatype, DatatypeOpApp}
import miniL4.typechecker.stdlibTyping.stdDataTypes._
import miniL4.typechecker._

object stdSubtypePairs {
  val ssp = SimpleSubtypePair
  val assp : (Name,Name) => SimpleSubtypePair = {case(x,y) => SimpleSubtypePair(AtomicDatatype(x),AtomicDatatype(y))}
  val sschain = SimpleSubtypeChain
  val asschain : Iterable[Name] => SimpleSubtypeChain = (x:Iterable[Name]) => SimpleSubtypeChain(x.view.map(AtomicDatatype(_)))
  val l = List[Datatype] _
  val s = Symbol(_)

  val atomicConstraints : TSet[SubtypingDec] = Set[SubtypingDec](
    asschain(List(sPosReal,sNonnegReal,sReal)),

    assp(s0,s01),
    assp(s1,s01),
    assp(s0,s("[0,1)")),
    assp(s1,s("(0,1]")),
    assp(s01,s("[0,1]")),

    assp(s("[0,1]"),sNonnegReal),
    assp(s("(0,1]"), sPosReal),
    asschain(List(s("(0,1)"), s("[0,1)"), s("[0,1]"))),
    asschain(List(s("(0,1)"), s("(0,1]"), s("[0,1]"))),
    asschain(List(s("(0,1)"), s("[0,1)"), s("[0,1]")))
  )

  val BASIC_NUMERIC_DATATYPES : TSet[Name] = Set(
    sReal, sPosReal, sNonnegReal,
    s0, s1, s01,
    s("(0,1)"), s("[0,1)"), s("(0,1]"), s("[0,1]")
  )

  val basicSubtypesOfPosReal : TSet[Datatype] = Set(
    s("(0,1]"), s("(0,1)"), s1
  ).map(AtomicDatatype(_))

  val basicSubtypesOfNonnegReal : TSet[Datatype] = basicSubtypesOfPosReal ++ Set(
    sPosReal, s("[0,1]"), s("[0,1)"), s0, s01
  ).map(AtomicDatatype(_))

  val dtReal = AtomicDatatype(sReal)
  val dtPosReal = AtomicDatatype(sPosReal)
  val dtNonnegReal = AtomicDatatype(sNonnegReal)

  val STD_SUBTYPE_PAIRS : TSet[(Datatype,Datatype)] =
    atomicConstraints.flatMap(_.pairs) ++
    basicSubtypesOfPosReal.map((_,posRealDType)) ++
    basicSubtypesOfNonnegReal.map((_,nonnegRealDType))
}