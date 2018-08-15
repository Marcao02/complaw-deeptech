package miniL4.typechecker.stdlibTyping

import miniL4.ast.astutil._
import stdDataTypes._
import miniL4.typechecker._
import SubtypePairPattern.sspp

object stdParametricSubtypingPairs {

  private val x1 = ConstantMatchVar('x1,sReal)
  private val x2 = ConstantMatchVar('x2,sReal)
  private val y1 = ConstantMatchVar('y1,sReal)
  private val y2 = ConstantMatchVar('y2,sReal)

  // (y1,y2) ⊆ (x1,x2) when x1 ≤ y1 and x2 ≥ y2
  // and likewise for the other three interval types
  val intervalSubtypeDecs : List[SubtypePairPattern] =
    List('CCInterval, 'OCInterval, 'COInterval, 'OOInterval).map( intervalTypeOp =>
      DependentSubtypePairPattern(
        DependentDatatypeOpAppPattern(intervalTypeOp, List(y1,y2)),
        DependentDatatypeOpAppPattern(intervalTypeOp, List(x1,x2)),
        and((leq(x1,y1), geq(x2,y2)))
      )) ++
      sspp(
        DependentDatatypeOpAppPattern('CCInterval, List(x1,x2)),
        DependentDatatypeOpAppPattern('COInterval, List(x1,x2)),
        DependentDatatypeOpAppPattern('OOInterval, List(x1,x2))
      ) ++
      sspp(
        DependentDatatypeOpAppPattern('CCInterval, List(x1,x2)),
        DependentDatatypeOpAppPattern('OCInterval, List(x1,x2)),
        DependentDatatypeOpAppPattern('OOInterval, List(x1,x2))
      )

}

/*
(types Real Bool ⟘)
(typeops (OOInterval CCInterval OCInterval COInterval) (Real -> Real -> Type))
(typeops (FinSet) (forallType α (α -> Type)))
(typeops (FinSet) (forallType α (α -> α -> Type)))
(typeops (FinSet) (forallType α (α -> α -> α -> Type)))
(typeops (FinSet) (forallType α (α -> α -> α -> α -> Type)))
(OOInterval l r) ≤ {(OCInterval l r) (COInterval l r)} ≤ (CCInterval l r) ≤ Real
(FinSet Real x) ≤ (OOInterval y1 y2)  (if (and (y1 < x) (x > y2)))
(FinSet Real x) ≤ (COInterval y1 y2)  (if (and (y1 <= x) (x > y2)))
(FinSet Real x) ≤ (OCInterval y1 y2)  (if (and (y1 < x) (x >= y2)))
(FinSet Real x) ≤ (CCInterval y1 y2)  (if (and (y1 <= x) (x >= y2)))
(FinSet Real x) ≤ (FinSet Real x y) ≤ (FinSet Real x y z)
(FinSet Real x y) = (FinSet Real y x)
(FinSet Real x y z) = (FinSet Real y x z) = (FinSet Real y z x)
 */
//  val ratioBasicSubtypes = PSDForEachDatatypeIn(Set(dtReal, dtNonnegReal, dtPosReal), s =>
//    PSDForEachDatatypeIn(basicSubtypesOfPosReal, sPos =>
//      SimpleSubtypePair(DatatypeOpApp('Ratio,List(s,sPos)), s)
//    )
//  )
//  val ratioRatioSubtypes = PSDForEachDatatypeIn(basicSubtypesOfPosReal, sPos =>
//    SimpleSubtypeChain(List(
//      DatatypeOpApp('Ratio,List(dtPosReal,sPos)), DatatypeOpApp('Ratio,List(dtNonnegReal,sPos)), DatatypeOpApp('Ratio,List(dtReal,sPos))
//      /*, ... and many more*/
//    )
//    )
//  )