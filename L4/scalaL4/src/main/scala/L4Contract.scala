import com.github.nscala_time.time.Imports._
import com.github.nscala_time.time.Imports.{DateTime => DateTimeFromLib}
import org.joda.time.Period

import Sort._
import Term._
import handy_term_constructors._
import Statement._
import EventRule._
import TypeAliases._
import FnType._

import Deontic.Deontic
import scala.collection.{SortedMap, SortedSet, mutable}


class SyntaxError(val msg:String) extends Exception(msg)

object TypeAliases {
  type DateTime = DateTimeFromLib
  type TimeDelta = Period
  type TimeUnit = String
  type OrderedMap[K, V] = SortedMap[K, V]
  type OrderedSet[V] = SortedSet[V]

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
    case class StateVar(name: Symb.StateVar, primed: Boolean) extends BoundVar
    case class LocalVar(name: Symb.LocalVar) extends BoundVar

    val tlast_env : EnvVar = EnvVar(Symb.tlast_env)
    val tnext_env : EnvVar= EnvVar(Symb.tnext_env)
    val tlast_state : StateVar= StateVar(Symb.tlast_state, false)
    val tnext_eparam : EventParam= EventParam(Symb.tnext_eparam)
  }

  object handy_term_constructors {
    def plus(t1:Term, t2:Term) : Term = {
      FnApp("plus",List(t1,t2))
    }
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
  val timestamp = Atomic(Symb.timestamp)
  val bool = Atomic(Symb.bool)
}

object FnType {
  type SortSeq = Seq[Sort]
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

case class L4EventDec(name: Symb.EventDec, destSit: Symb.Situation, paramNames: OrderedSet[Symb.EventParam],
                      transform: Block,
                      hasFollowingSituation: Boolean)

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
  case class InternalEventRule(basic: BasicEventRule, paramSetters: OrderedMap[Symb.EventParam, Term], timeTrigger: Term, triggerType: TimeTrigger.TimeTrigger) extends EventRule

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
                       fnBasis: Map[Symb.Fn, FnType],
                       startSituation: Symb.Situation,
                       contractParamSorts: OrderedMap[Symb.ContractParam, Sort],
                       stateVarInitVals : OrderedMap[Symb.StateVar, Term],
                       stateVarSorts : OrderedMap[Symb.StateVar, Sort],
                       situations : OrderedMap[Symb.Situation,Situation],
                       eventDecs : OrderedMap[Symb.EventDec, L4EventDec],
                       eventParamSorts : OrderedMap[Symb.EventParam, Sort] ) {
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
                      case TimeConstraintShorthandKeyword.before_timedelta_contract => InternalEventRule(new_basic, SortedMap(), timeterm, TimeTrigger.at_timedelta_contract)
                      case TimeConstraintShorthandKeyword.before_timedelta_event => InternalEventRule(new_basic, SortedMap(), timeterm, TimeTrigger.at_timedelta_event)
                      case TimeConstraintShorthandKeyword.before_datetime => InternalEventRule(new_basic, SortedMap(), timeterm, TimeTrigger.on_datetime)
                      case TimeConstraintShorthandKeyword.within_timedelta_contract => InternalEventRule(new_basic, SortedMap(), plus(timeterm, Literal.Real(1)), TimeTrigger.at_timedelta_contract)
                      case TimeConstraintShorthandKeyword.within_timedelta_event => InternalEventRule(new_basic, SortedMap(), plus(timeterm,Literal.Real(1)), TimeTrigger.at_timedelta_event)
                      case TimeConstraintShorthandKeyword.within_datetime => InternalEventRule(new_basic, SortedMap(), plus(timeterm,Literal.Real(1)), TimeTrigger.on_datetime)
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
      (event_sit_pair._1, L4EventDec(event_sit_pair._1, event_sit_pair._2, SortedSet(), List(), hasFollowingSituation = true))).toMap
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


object INeedThis extends App {
  println("Hello, World!")
}