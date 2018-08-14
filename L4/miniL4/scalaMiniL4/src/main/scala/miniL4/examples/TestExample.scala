package miniL4.examples

import miniL4.ast.Contract
import miniL4.interpreter.Trace.Trace

trait TestExample {
  val contract : Contract
  val traces : Seq[Trace]
  val exceptionTraces : Seq[Trace]
}
