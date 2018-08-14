package miniL4.typechecker.stdlibTyping

import miniL4.ast.{Datatype, DatatypeOpApp, Term}
import Datatype.datatypeOpAppHelper

object stdDataTypeOps {
  val soDimensioned = 'Dimensioned
  val soRatio = 'Ratio
  val soTuple = 'Tuple

  def tupleApp(args: Datatype*) : Datatype = datatypeOpAppHelper(soTuple)(args)
  def ratioApp(args: Datatype*) : Datatype = datatypeOpAppHelper(soRatio)(args)
}
