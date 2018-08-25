package indy.sexpr

import indy.srcLocation.LCPos
import indy.srcLocation.Loc.LinPos

object exceptions {
  // can't enforce this because of infix notation
  // case class NoHeadTokenError(ch:Seq[SExpr], lcarea:LCArea) extends Exception(s"Default constructor for BrackExpr requires that the first child element is a Token ${if(ch.length == 0) "but got no children" else s"but got head ${ch(0)}" }. See ${lcarea}.")
  // can't enforce this because of (if block else ())
  // case class EmptyBrackExprError(lcarea:LCArea) extends Exception(s"Default constructor for BrackExpr requires at least one child. See ${lcarea}.")

  case class UnbalancedException(expected: Option[Char], found: Option[Char], linpos: LinPos) extends Exception(s"Expected ${expected} but found ${found} at linear pos ${linpos}.")

  case class UnbalancedExceptionLC(expected: Option[Char], found: Option[Char], lcpos: LCPos) extends Exception(s"Expected ${expected} but found ${found} at ${lcpos}.")
}