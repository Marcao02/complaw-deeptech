package miniL4.typechecker

import miniL4.typechecker.DatatypePattern.DatatypePatSubst


// I'd like the third element `sidecondition` of a SubtypePairPattern to just be a Term, which can be evaluated with a function
// evalSimpleTerm(term:Term, subst:TMap[Name,Term])
// which assumes (exception otherwise) that NiT subterms are in subst, and no TypeAnnotation subterms for now,
// since we won't be typechecking these terms for now.
case class SubtypePairPattern(left:DatatypePattern, right:DatatypePattern,
                              sidecondition:DatatypePatSubst => Boolean) {
}

