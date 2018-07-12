
/*
L4's "immediately" will have to mean after ε timedelta.

*/


import Sort._
import Term._
import handy_term_constructors._
import Statement._
import EventRule._
import TypeAliases._
import FnType._
import Sort.SortSeq
import Deontic.Deontic

import scala.collection.mutable

class SyntaxError(val msg:String) extends Exception(msg)

object TypeAliases {
  type DateTime = Any
  type TimeDelta = Any
  type TimeUnit = String
  type OrderedDict[K, V] = Map[K, V]
  type OrderedSet[V] = Seq[V]

  type TermSeq = Seq[Term]
  type SortSeq = Seq[Sort]
  type Block = Seq[Statement]
}

object Symb {
  type SortOp = String
  type AtomicSort = String

  type EnvVar = String
  type ContractParam = String

  type LocalVar = String
  type Fn = String
  type StateVar = String

  type Situation = String
  type EventDec = String
  type EventParam = String
  type RuleParam = String
  type Role = String

  val tlast_env : EnvVar = "td_last"
  val tnext_env : EnvVar = "td_next"

  val tlast_state : StateVar = "td_last"
  val tnext_eparam : EventParam = "td_next"

  val and : Fn = "and"
  val gt_ts : Fn = "gt_ts"

  val timestamp : AtomicSort = "timestamp"
  val bool : AtomicSort = "bool"


}

object Term {
  sealed abstract class Term

  case class FnApp(fnName: Symb.Fn, args: TermSeq) extends Term

  object BoundVar {

    sealed abstract class BoundVar extends Term

    case class RuleParam(name: Symb.RuleParam) extends BoundVar

    case class EventParam(name: Symb.EventParam) extends BoundVar

    case class EnvVar(name: Symb.EnvVar) extends BoundVar

    case class StateVar(name: Symb.StateVar) extends BoundVar

    case class LocalVar(name: Symb.LocalVar) extends BoundVar

    val tlast_env : EnvVar = EnvVar(Symb.tlast_env)
    val tnext_env : EnvVar= EnvVar(Symb.tnext_env)
    val tlast_state : StateVar= StateVar(Symb.tlast_state)
    val tnext_eparam : EventParam= EventParam(Symb.tnext_eparam)
    
    val timestamp = Sort.Atomic(Symb.timestamp)

    val bool = Sort.Atomic(Symb.bool)
  }

  object handy_term_constructors {
    def plus(t1:Term, t2:Term) : Term = {
      FnApp("plus",List(t1,t2))
    }
//    val s_eq : (STerm, STerm) => STerm
//    val s_minus : (STerm, STerm) => STerm
//    val s_lt : (STerm, STerm) => STerm
//    val s_gt_ts : (STerm, STerm) => STerm
//    val s_and : (STerm, ...) => STerm
  }

  object Literal {
    sealed abstract class Literal extends Term

    case class Bool(lit: Boolean) extends Literal

    case class Int(lit: Int) extends Literal

    case class Real(lit: Float) extends Literal

    case class DateTime(lit: DateTime) extends Literal

    case class TimeDelta(lit: TimeDelta) extends Literal

  }

}


object Sort {
  type SortSeq = Seq[Sort]

  sealed abstract class Sort

  case class Atomic(name: Symb.AtomicSort) extends Sort

  case class OpApp(sortOp: Symb.SortOp, args: SortSeq) extends Sort

}

object FnType {

  case class FnType(dom: SortSeq, ran: Sort)

  case class OverloadedFnType(parts: Seq[FnType])

}
object Statement {
  sealed abstract class Statement

  case class LocalAssign(name: Symb.LocalVar, rhs: Term) extends Statement

  case class StateVarAssign(name: String, rhs: Term) extends Statement

  case class IfElse(test: Term, trueBranch: Block, falseBranch: Block) extends Statement

  case class Prove(conjecture: Term) extends Statement

}


case class Situation(name:Symb.Situation, handlerSet:Seq[EventRule])

case class L4EventDec(name: Symb.EventDec, destSit: String, paramNames: OrderedSet[Symb.EventParam],
                      transform: Block,
                      followingSituation: Option[Symb.Situation])

object EventRule {

  object TimeConstraintShorthandKeyword extends Enumeration {
    type TimeConstraintShorthandKeyword = Value
    val before_timedelta_contract : TimeConstraintShorthandKeyword = Value
    val before_timedelta_event : TimeConstraintShorthandKeyword = Value
    val before_datetime : TimeConstraintShorthandKeyword = Value
    val within_timedelta_contract : TimeConstraintShorthandKeyword = Value
    val within_timedelta_event : TimeConstraintShorthandKeyword = Value
    val within_datetime : TimeConstraintShorthandKeyword = Value

  }

  import TimeConstraintShorthandKeyword.TimeConstraintShorthandKeyword
  case class TimeConstraintShorthand(keyword:TimeConstraintShorthandKeyword, timeterm:Term)

  sealed abstract class EventRule

  case class BasicEventRule(eventTypeName: Symb.EventDec, enabledGuard: Term)

  case class ExternalEventRule(basic: BasicEventRule, roles: OrderedSet[Symb.Role], paramConstraint: Term,
                               timeConstraint: Either[Term,TimeConstraintShorthand], deontic: Deontic) extends EventRule

  case class InternalEventRule(basic: BasicEventRule, paramSetters: OrderedDict[Symb.EventParam, Term], timeTrigger: Term, triggerType: TimeTrigger.TimeTrigger) extends EventRule

  object TimeTrigger extends Enumeration {
    type TimeTrigger = Value
    val at_timedelta_contract : TimeTrigger = Value //  relative to start of contract
    val at_timedelta_event : TimeTrigger = Value // relative to timestamp of last event (i.e. situation entrance)
    val on_datetime : TimeTrigger = Value
  }

  object Deontic extends Enumeration {
    type Deontic = Value
    val may : Deontic = Value
    val must : Deontic = Value
  }
}

case class L4Contract(
                       timeunit:TimeUnit,
                       fnBasis: OrderedDict[Symb.Fn, FnType],
                       startSituation: Symb.Situation,
                       contractParamSorts: OrderedDict[Symb.ContractParam, Sort],
                       stateVarInitVals : OrderedDict[Symb.StateVar, Term],
                       stateVarSorts : OrderedDict[Symb.StateVar, Sort],
                       situations : OrderedDict[Symb.Situation,Situation],
                       eventDecs : OrderedDict[Symb.EventDec, L4EventDec],
                       eventParamSorts : OrderedDict[Symb.EventParam, Sort] ) {
	lazy val externalEventRules : Iterable[ExternalEventRule] =
		for{
			sit <- this.situations.values
			erule <- sit.handlerSet
			if erule.isInstanceOf[ExternalEventRule]
		} yield erule.asInstanceOf[ExternalEventRule]

  lazy val internalEventRules : Iterable[InternalEventRule] =
		for{
			sit <- this.situations.values
			erule <- sit.handlerSet
			if erule.isInstanceOf[InternalEventRule]
		} yield erule.asInstanceOf[InternalEventRule]
}

object transformations {


  def eliminateMust(p:L4Contract) : L4Contract = {
    val breach_events_situations_used : mutable.Set[(Symb.EventDec,Symb.Situation)] = new mutable.HashSet[(Symb.EventDec,Symb.Situation)]

    val new_situations = p.situations.mapValues( sit => {
      Situation(sit.name, sit.handlerSet.flatMap( erule => {
        erule match {
          case InternalEventRule(_,_,_,_) => List(erule)
          case ExternalEventRule(basic,roles,paramConstraint,timeConstraint,deontic) => {
            deontic match {
              case Deontic.may => List(erule)
              case Deontic.must => List(
                ExternalEventRule(basic,roles,paramConstraint,timeConstraint,Deontic.may),
                timeConstraint match {
                  case Left(_) => {
                    throw new SyntaxError("Required to use one of the shorthand forms for the time constraint of a `must` rule.")
                  }
                  case Right(TimeConstraintShorthand(keyword, timeterm)) => {
                    val breach_event_symb = "Enter_Breach_" + roles.mkString("_")
                    val breach_situation_symb = "Breach_" + roles.mkString("_")
                    breach_events_situations_used.add((breach_event_symb,breach_situation_symb))
                    val new_basic = BasicEventRule(breach_event_symb, basic.enabledGuard)
                    keyword match {
                      case TimeConstraintShorthandKeyword.before_timedelta_contract => InternalEventRule(new_basic, Map(), timeterm, TimeTrigger.at_timedelta_contract)
                      case TimeConstraintShorthandKeyword.before_timedelta_event => InternalEventRule(new_basic, Map(), timeterm, TimeTrigger.at_timedelta_event)
                      case TimeConstraintShorthandKeyword.before_datetime => InternalEventRule(new_basic, Map(), timeterm, TimeTrigger.on_datetime)
                      case TimeConstraintShorthandKeyword.within_timedelta_contract => InternalEventRule(new_basic, Map(), plus(timeterm, Literal.Real(1)), TimeTrigger.at_timedelta_contract)
                      case TimeConstraintShorthandKeyword.within_timedelta_event => InternalEventRule(new_basic, Map(), plus(timeterm,Literal.Real(1)), TimeTrigger.at_timedelta_event)
                      case TimeConstraintShorthandKeyword.within_datetime => InternalEventRule(new_basic, Map(), plus(timeterm,Literal.Real(1)), TimeTrigger.on_datetime)
                    }
                  }
                }
              )
            }
          }
        }
      }))
    })


    val breach_eventDecs = p.eventDecs.++( breach_events_situations_used.map( event_sit_pair =>
      (event_sit_pair._1, L4EventDec(event_sit_pair._1, event_sit_pair._2, List(), List(), Some(event_sit_pair._2)))).toMap
    )
    val breach_sitDecs = p.situations.++( breach_events_situations_used.map( event_sit_pair =>
      (event_sit_pair._2, Situation(event_sit_pair._2, List()))).toMap
    )


    L4Contract(
      p.timeunit,
      p.fnBasis,
      p.startSituation,
      p.contractParamSorts,
      p.stateVarInitVals,
      p.stateVarSorts,
      new_situations ++ breach_sitDecs,
      p.eventDecs ++ breach_eventDecs,
      p.eventParamSorts
    )
  }
}

//object DoINeedThis extends App {
//
//}