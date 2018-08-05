package miniL4.interpreter

object Trace {
  type Trace = Seq[L4Event]

  def minimalTraceString(trace:Trace) : String = {
    //    trace.map(_.eventName.toString()).fold("")((x,y) => s"$x -> $y")
    if(trace.isEmpty)
      "<empty trace>"
    else
      trace.map(_.eventName.toString()).reduce((x,y) => s"$x -> $y")
  }
}

//class TestTrace(trace:Trace, outcome:EvalOutcome) {
//
//}