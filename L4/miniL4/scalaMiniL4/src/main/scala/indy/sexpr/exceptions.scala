package indy.sexpr

object exceptions {

  case class UnbalancedException(expected: Option[Char], found: Option[Char], linpos: LinPos) extends Exception(s"Expected ${expected} but found ${found} at linear pos ${linpos}.")

  case class UnbalancedExceptionLC(expected: Option[Char], found: Option[Char], lcpos: LCPos) extends Exception(s"Expected ${expected} but found ${found} at ${lcpos}.")
}