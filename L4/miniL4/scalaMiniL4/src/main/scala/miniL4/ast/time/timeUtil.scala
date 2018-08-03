package miniL4.ast.time

import miniL4.Real
import miniL4.ast.{TimeDeltaLit, TimeDeltaUnit}

object timeUtil {
  def at_m(x:Real) : TimeTrigger = AtTimeDelta(TimeDeltaLit(x,TimeDeltaUnit.m))
  def after_m(x:Real) : TimeTrigger = AfterTimeDelta(TimeDeltaLit(x,TimeDeltaUnit.m))
  def before_m(x:Real) : TimeConstraint = BeforeTimeDeltaSplit(TimeDeltaLit(x,TimeDeltaUnit.m))
  def within_m(x:Real) : TimeConstraint = WithinTimeDeltaSplit(TimeDeltaLit(x,TimeDeltaUnit.m))
}
