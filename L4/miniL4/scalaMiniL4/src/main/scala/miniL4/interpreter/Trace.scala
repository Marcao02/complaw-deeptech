package miniL4.interpreter

object Trace {
  type Trace = Seq[L4Event]

  def minimalTraceString(trace:Trace) : String = {
    if(trace.isEmpty)
      "<empty trace>"
    else
      trace.map(_.eventName.toString()).reduce((x,y) => s"$x -> $y")
  }

  def traceStringWithEventParamsNoNamesNoTime(trace:Trace) : String = {
    if(trace.isEmpty)
      "<empty trace>"
    else
      trace.map(evt => {
        if( evt.paramVals.nonEmpty )
          s"${evt.eventName}${evt.paramVals.values.toList}"
        else
          s"${evt.eventName}"
      }).reduce((x,y) => s"$x -> $y")
  }
}

//class TestTrace(trace:Trace, outcome:EvalOutcome) {
//
//}