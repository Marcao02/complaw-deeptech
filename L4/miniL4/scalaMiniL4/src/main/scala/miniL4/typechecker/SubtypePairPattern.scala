package miniL4.typechecker

import miniL4.ast.Term

abstract sealed class SubtypePairPattern {
  val left:DatatypePattern
  val right:DatatypePattern
}

case class DependentSubtypePairPattern(left:DatatypePattern, right:DatatypePattern,
                                       sidecondition: Term) extends SubtypePairPattern

case class HigherOrderSubtypePairPattern(left:DatatypePattern, right:DatatypePattern,
                                         sidecondition: DatatypePatSubst => Boolean) extends SubtypePairPattern

