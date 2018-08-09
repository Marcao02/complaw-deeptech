package miniL4.typechecker

import miniL4.typechecker.DatatypePattern.DatatypePatSubst


case class SubtypePairPattern(left:DatatypePattern, right:DatatypePattern,
                              sidecondition:DatatypePatSubst => Boolean) {
}

