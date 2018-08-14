package miniL4.typechecker.stdlibTyping

import miniL4.ast.{AtomicDatatype, Datatype}

object stdDataTypes {
  val sBool = 'Bool
  val sTimeDelta = 'TimeDelta
  val sReal = 'Real
  val sBottom = '⟘
  val sNonnegReal = 'NonnegReal
  val sPosReal = 'PosReal

  val sO01O = Symbol("(0,1)")
  val sO01C = Symbol("(0,1]")
  val sC01O = Symbol("[0,1)")
  val sC01C = Symbol("[0,1]")
  val s0 = Symbol("{0}")
  val s1 = Symbol("{1}")
  val s01 = Symbol("{0,1}")

  val timeDeltaDType = AtomicDatatype(sTimeDelta)
  val realDType = AtomicDatatype(sReal)
  val boolDType = AtomicDatatype(sBool)
  val bottomDType = AtomicDatatype(sBottom)
  val posRealDType = AtomicDatatype(sPosReal)
  val nonnegRealDType = AtomicDatatype(sNonnegReal)

  val ALWAYS_INCLUDED : Set[Datatype] = Set(timeDeltaDType.asInstanceOf[Datatype], realDType, boolDType, bottomDType, posRealDType, nonnegRealDType) ++
        Set(sO01O, sO01C, sC01O, sC01C, s0, s1, s01).view.map(AtomicDatatype(_))

}
