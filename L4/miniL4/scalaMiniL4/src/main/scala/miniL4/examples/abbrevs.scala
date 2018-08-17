package miniL4.examples

import miniL4._
import indy.type_abbrevs._
import miniL4.ast.Statement.Block
import miniL4.ast.time.{NoTimeConstraint, TimeConstraint, TimeTrigger}
import miniL4.ast.{ToplevelNode, _}
import miniL4.interpreter.RTData.{RTBool, RTData, RTReal}

object abbrevs {
  def d(x:AnyVal) : RTData = x match {
    case _x:Real => RTReal(_x)
    case _x:Int => RTReal(_x.toDouble)
    case _x:Boolean => RTBool(_x)
    case _ => throw new L4ProjectError(s"$x is not a valid runtime data value.")
  }

  def ehd(eventName: Name, destSit: Name, stateTransform: Block = List(),
          paramsAndDatatypes: Seq[(Name,Datatype)] = List(), preconditions: Seq[Term] = List(), loc: Loc = NoLoc) =
    EventHandlerDef(eventName, destSit, stateTransform, paramsAndDatatypes, preconditions, loc)

  def sd(name: Name, eventRules: Seq[EventRule], preconditions: Seq[Term] = List(), loc: Loc = NoLoc) =
    SituationDef(name, eventRules, preconditions, loc)


  def eer(eventDefName: Name,
          roleIds: Seq[Name],
          timeConstraint: TimeConstraint = NoTimeConstraint(NoLoc),
          enabledGuard: Option[Term] = None,
          ruleparamNames: Seq[Name] = List(),
          paramConstraint: Option[Term] = None,
          loc:Loc = NoLoc) =
    ExternalEventRule(eventDefName, roleIds, timeConstraint, enabledGuard, ruleparamNames, paramConstraint, loc)

  def ier(eventDefName: Name, timeTrigger: TimeTrigger, enabledGuard: Option[Term] = None, ruleparamNames: Seq[Name] = List(), paramSetter: Seq[Term] = List(), loc:Loc = NoLoc) =
    InternalEventRule(eventDefName, timeTrigger, enabledGuard, ruleparamNames, paramSetter, loc)

  def sva(name: Name, rhs: Term, loc: Loc = NoLoc) = StateVarAssign(name, rhs, loc)

  def svd(name: Name, dtype: Datatype, initVal: Option[Term] = None, modifiers: Seq[Symbol] = List(), loc: Loc = NoLoc) =
    StateVarDef(name, dtype, initVal, modifiers, loc)

  val ntc = NoTimeConstraint()
}
