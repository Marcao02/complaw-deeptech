package miniL4.typechecker

import miniL4.ast.Term
import indy.type_abbrevs._
import indy.util._

abstract sealed class SubtypePairPattern {
  val left:DatatypePattern
  val right:DatatypePattern
}

case class SimpleSubtypePairPattern(left:DatatypePattern, right:DatatypePattern) extends SubtypePairPattern

case class DependentSubtypePairPattern(left:DatatypePattern, right:DatatypePattern,
                                       sidecondition: Term) extends SubtypePairPattern

case class HigherOrderSubtypePairPattern(left:DatatypePattern, right:DatatypePattern,
                                         sidecondition: DatatypePatSubst => Boolean) extends SubtypePairPattern

object SubtypePairPattern {
  def sspp(pats:DatatypePattern*) : Iterable[SimpleSubtypePairPattern] = seqToPairs(pats).map {case (x,y) => SimpleSubtypePairPattern(x,y)}

}
