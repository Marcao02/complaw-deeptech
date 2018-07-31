import com.github.nscala_time.time.Imports._
import com.github.nscala_time.time.Imports.{DateTime => DateTimeFromLib}
import org.joda.time.Period
import scala.collection.{SortedMap, SortedSet, mutable}

import SortOcc._
import Term._
import handy_term_constructors._
import Statement._
import EventRule._
import EventRule.TimeConstraintShorthandType.TimeConstraintShorthandType
//import FixedSymb
import TypeAliases._
import FnType._



object TypingLibrary {
  /*
  Eventually each FixedSymb.Fn gets assigned an OverloadedFnType.
  An OverloadedFnType is a scala function of type SortSeq -> Maybe[SortOcc].
  */

}

// namespace for code dealing with symbols whose meaning/usage is completely
// fixed by the L4 semantics.
// this will probably end up being a file.
object FixedSymb {
  val tlast_env = 'td_last
  val tnext_env = 'td_next
  val tlast_state = 'td_last
  val tnext_eparam = 'td_next

  val and = 'and
  val gt_ts = 'gt_ts
  val plus = 'plus

  val timestamp = 'timestamp
  val bool = 'bool
  val real = 'real

  val fns : Set[Symbol] = Set(and, gt_ts)
  val env_vars : Set[Symbol] = Set(tlast_env, tnext_env)
  val atomic_sorts : Set[Symbol] = Set(bool, real, timestamp)
}

// namespace for code dealing with symbols declared within a contract
// this will probably end up being a file.
object ContractSymbDec {
  sealed abstract class ContractSymbDec(symb: Symbol, loc:Loc) extends Locatable

  // the .sort field is the definition of this custom sort symbol
  case class CustSort(symb: Symbol, sort:SortOcc, loc:Loc) extends ContractSymbDec(symb,loc)
  // later: case class CustFn(symb: Symbol, loc:Loc) extends ContractSymbDec
  // sort never inferred:
  case class ContractParam(symb: Symbol, sort:SortOcc, loc:Loc) extends ContractSymbDec(symb,loc)
  case class EventParam(symb: Symbol, sort:SortOcc, loc:Loc) extends ContractSymbDec(symb,loc)
  case class StateVar(symb: Symbol, sort:SortOcc, loc:Loc) extends ContractSymbDec(symb,loc)
  // sort possibly inferred, so not required:
  case class LocalVar(symb: Symbol, sort:Option[SortOcc], loc:Loc) extends ContractSymbDec(symb,loc)
  // sort always inferred from the corresponding EventType dec, so can't be given:
  case class RuleParam(symb: Symbol, loc:Loc) extends ContractSymbDec(symb,loc)

  // untyped symbols:
  case class Role(symb: Symbol, loc:Loc) extends ContractSymbDec(symb,loc)
  case class EventType(symb: Symbol, loc:Loc) extends ContractSymbDec(symb,loc)
  case class Situation(symb: Symbol, loc:Loc) extends ContractSymbDec(symb,loc)

  //  // these are introduced in one of the compilation steps.
  //  // they thus don't really have source locations.
  //  val tlast_state = (filepath: String) => StateVar('td_last, SortOcc.Atomic(FixedSymb.real), GeneratedSrcLoc(filepath))
  //  val tnext_eparam = (filepath: String) => EventParam('td_next, SortOcc.Atomic(FixedSymb.real), GeneratedSrcLoc(filepath))
}

object Term {
  sealed abstract class Term(loc:Loc) extends  Locatable

  case class FnApp(fnName: Symbol, args: TermSeq, loc:Loc) extends Term(loc)

  object TermSymbOcc {
    sealed abstract class TermSymbOcc(symb:Symbol, loc:Loc) extends Term(loc)

    case class SortOp(symb:Symbol, loc:Loc) extends TermSymbOcc(symb,loc)
    case class AtomicSort(symb:Symbol, loc:Loc) extends TermSymbOcc(symb,loc)
    case class EnvVar(symb:Symbol, loc:Loc) extends TermSymbOcc(symb,loc)
    case class Fn(symb:Symbol, loc:Loc) extends TermSymbOcc(symb,loc)
    case class CustSort(symb:Symbol, loc:Loc) extends TermSymbOcc(symb,loc)
    case class ContractParam(symb:Symbol, loc:Loc) extends TermSymbOcc(symb,loc)
    case class EventParam(symb:Symbol, loc:Loc) extends TermSymbOcc(symb,loc)
    case class LocalVar(symb:Symbol, loc:Loc) extends TermSymbOcc(symb,loc)
    case class Role(symb:Symbol, loc:Loc) extends TermSymbOcc(symb,loc)
    case class RuleParam(symb:Symbol, loc:Loc) extends TermSymbOcc(symb,loc)
    case class StateVar(symb:Symbol, primed:Boolean, loc:Loc) extends TermSymbOcc(symb,loc)
    case class Situation(symb:Symbol, loc:Loc) extends TermSymbOcc(symb,loc)

    // maybe later
    // case class CustFn(symb:ContractSymbDec.CustFn, loc:Loc) extends TermSymbOcc

    // these are used often enough in transformations that we introduce shorthands:
    val tlast_env_occ = (loc:Loc) => EnvVar(FixedSymb.tlast_env, loc)
    def tnext_env_occ = (loc:Loc) => EnvVar(FixedSymb.tnext_env, loc)

    def tlast_state_occ = (loc:Loc) => StateVar(FixedSymb.tlast_state, primed=true, loc)
    def tnext_eparam_occ = (loc:Loc) => EventParam(FixedSymb.tnext_eparam, loc)
  }

  object handy_term_constructors {
    def plus(t1:Term, t2:Term, loc:Loc) : Term = {
      FnApp('plus, List(t1,t2), loc)
    }
  }

  object Literal {
    sealed abstract class Literal(loc:Loc) extends Term(loc) with Locatable
    case class Bool(lit: Boolean, loc:Loc) extends Literal(loc)
    case class Int(lit: Int, loc:Loc) extends Literal(loc)
    case class Real(lit: Float, loc:Loc) extends Literal(loc)
    case class DateTime(lit: DateTime, loc:Loc) extends Literal(loc)
    case class TimeDelta(lit: TimeDelta, loc:Loc) extends Literal(loc)
  }
}




object SortOcc {
  sealed abstract class SortOcc
  case class Atomic(sort: Symbol) extends SortOcc
  case class OpApp(sortOp: Symbol, args: TypeAliases.SortOccSeq) extends SortOcc
}

object FnType {
  type SortSeq = Seq[SortOcc]
  case class FnType(dom: SortSeq, ran: SortOcc)
  case class OverloadedFnType(parts: Seq[FnType])
}

object Statement {
  sealed abstract class Statement
  case class LocalAssign(symb: Symbol, rhs: Term) extends Statement
  case class StateVarAssign(symb: Symbol, rhs: Term) extends Statement
  case class IfElse(test: Term, trueBranch: Block, falseBranch: Block) extends Statement
  case class Prove(conjecture: Term) extends Statement
}

case class Situation(name: Symbol, handlerSet:Seq[EventRule])

case class L4EventType(name: Symbol, destSit: Symbol, paramNames: List[Symbol],
                       transform: Block,
                       hasFollowingSituation: Boolean)

object EventRule {
  object TimeConstraintShorthandType extends Enumeration {
    type TimeConstraintShorthandType = Value
    val before_timedelta_contract : TimeConstraintShorthandType = Value
    val before_timedelta_event : TimeConstraintShorthandType = Value
    val before_datetime : TimeConstraintShorthandType = Value
    // DW NTS: the within* forms use +1 in compilation
    val within_timedelta_contract : TimeConstraintShorthandType = Value
    val within_timedelta_event : TimeConstraintShorthandType = Value
    val within_datetime : TimeConstraintShorthandType = Value

  }

  case class TimeConstraintShorthand(keyword:TimeConstraintShorthandType, timeterm:Term)

  sealed abstract class EventRule
  case class BasicEventRule(eventTypeName: Symbol, enabledGuard: Term)
  case class ExternalEventPermittedRule( basic: BasicEventRule,
                                         roles: List[ContractSymbDec.Role],
                                         paramConstraint: Term,
                                         timeConstraint: Either[Term,TimeConstraintShorthand]) extends EventRule
  case class ExternalEventObligatedRule( basic: BasicEventRule,
                                         roles: List[ContractSymbDec.Role],
                                         paramConstraint: Term,
                                         timeConstraint: TimeConstraintShorthand) extends EventRule
  case class InternalEventRule( basic: BasicEventRule,
                                paramSetters: Map[Symbol, Term],
                                timeTrigger: Term,
                                triggerType: TimeTrigger.TimeTrigger) extends EventRule

  object TimeTrigger extends Enumeration {
    type TimeTrigger = Value
    val at_timedelta_contract : TimeTrigger = Value //  relative to start of contract
    val at_timedelta_event : TimeTrigger = Value // relative to timestamp of last event (i.e. situation entrance)
    val on_datetime : TimeTrigger = Value
  }


}

// might not end up using this class, in favor of L4ContractLinked
case class L4ContractParsed(
                       timeunit:TimeUnit,
                       fnBasis: Map[Symbol, FnType],
                       startSituation: Symbol,
                       contractParamSorts: OrderedMap[Symbol, SortOcc],
                       stateVarInitVals : OrderedMap[Symbol, Term],
                       stateVarSorts : OrderedMap[Symbol, SortOcc],
                       situations : OrderedMap[Symbol,Situation],
                       eventDecs : OrderedMap[Symbol, L4EventType],
                       eventParamSorts : OrderedMap[Symbol, SortOcc] ) {
  lazy val externalEventRules : Iterable[ExternalEventPermittedRule] =
    for{
      sit <- this.situations.values
      erule <- sit.handlerSet
      if erule.isInstanceOf[ExternalEventPermittedRule]
    } yield erule.asInstanceOf[ExternalEventPermittedRule]

  lazy val internalEventRules : Iterable[InternalEventRule] =
    for{
      sit <- this.situations.values
      erule <- sit.handlerSet
      if erule.isInstanceOf[InternalEventRule]
    } yield erule.asInstanceOf[InternalEventRule]
}

object correctnessChecks {
//  def noDateTimeVars(p:L4ContractParsed)

}


object transformations {
  /*
    NO! Transformations should be done on L4ContractLinked.

   */

  def eliminateMust(p:L4ContractParsed) : L4ContractParsed = {
    val breach_events_situations_used : mutable.Set[(Symbol, Symbol)] = new mutable.HashSet[(Symbol, Symbol)]

    val new_situations = p.situations.mapValues( sit => {
      Situation(sit.name, sit.handlerSet.flatMap( (erule: EventRule) => {
        erule match {
          case InternalEventRule(_,_,_,_) => List(erule)
          case ExternalEventPermittedRule(_,_,_,_) => List(erule)
          case ExternalEventObligatedRule(basic,roles,paramConstraint, timeConstraint) => { List(
            ExternalEventPermittedRule(basic,roles,paramConstraint, Right(timeConstraint)),
            {
              val TimeConstraintShorthand(keyword, timeterm) = timeConstraint
              val breach_event_symb = Symbol("Enter_Breach_" + roles.mkString("_"))
              val breach_situation_symb = Symbol("Breach_" + roles.mkString("_"))
              breach_events_situations_used.add((breach_event_symb, breach_situation_symb))
              val new_basic = BasicEventRule(breach_event_symb, basic.enabledGuard)
              val param_setters_TODO = Map.empty[Symbol,Term]
              val loc : Loc = timeterm.loc
              keyword match {
                case TimeConstraintShorthandType.before_timedelta_contract =>
                  InternalEventRule(new_basic, param_setters_TODO, timeterm, TimeTrigger.at_timedelta_contract)
                case TimeConstraintShorthandType.before_timedelta_event =>
                  InternalEventRule(new_basic, param_setters_TODO, timeterm, TimeTrigger.at_timedelta_event)
                case TimeConstraintShorthandType.before_datetime =>
                  InternalEventRule(new_basic, param_setters_TODO, timeterm, TimeTrigger.on_datetime)
                case TimeConstraintShorthandType.within_timedelta_contract =>
                  InternalEventRule(new_basic, param_setters_TODO, plus(timeterm, Literal.Real(1,loc), loc), TimeTrigger.at_timedelta_contract)
                case TimeConstraintShorthandType.within_timedelta_event =>
                  InternalEventRule(new_basic, param_setters_TODO, plus(timeterm,Literal.Real(1,loc), loc), TimeTrigger.at_timedelta_event)
                case TimeConstraintShorthandType.within_datetime =>
                  InternalEventRule(new_basic, param_setters_TODO, plus(timeterm,Literal.Real(1,loc), loc), TimeTrigger.on_datetime)
              }
            })
          }
        }
      }))
    })

    val breach_eventDecs = p.eventDecs.++( breach_events_situations_used.map( event_sit_pair => {
        val param_names_TODO = List.empty[Symbol]
        (event_sit_pair._1, L4EventType(event_sit_pair._1, event_sit_pair._2, param_names_TODO, List(), true))
      }).toMap
    )
    val breach_sitDecs = p.situations.++( breach_events_situations_used.map( event_sit_pair =>
      (event_sit_pair._2, Situation(event_sit_pair._2, List()))).toMap
    )

    L4ContractParsed(
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
