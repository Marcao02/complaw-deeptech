package miniL4.ast

import TimeDeltaUnit.TimeDeltaUnit
import miniL4.{Name, Real, seqmapHasKey}

// NOTE that these definitions aren't actually nested. It's just conceptual nesting.

abstract sealed class Term(loc: Loc) extends ASTNode(loc) {}

  /* Name in Term */
  case class NiT(name: Name, loc: Loc = NoLoc) extends Term(loc) {
    //  def sort(self, info:L4ContractTyping) : Sort:

    def defn(link:ContractLinking) : TermBinderO = {
      if( link.stateVarDefs.contains(this.name) ) {
        StateVarBinderO(link.stateVarDefs(this.name))
      }
      else {
        // then we walk up the AST looking for a LetIn expression.
        var cur : ASTNode = this
        while(true) {
          cur match {
            case LetIn(defs,_,_) => {
              defs.foreach({ case (name2, term) => {
                if (name2 == this.name)
                  return LetInBinderO(cur.asInstanceOf[LetIn])
              }})
            }
            case ehd@EventHandlerDef(_, _, _, paramAndSorts, _, _) => {
              if(seqmapHasKey(paramAndSorts, this.name)) {
                return EventHandlerParamBinderO(ehd)
              }
            }
            case er:EventRule => {
              if(er.ruleparamNames.contains(this.name)) {
                return EventRuleParamBinderO(er)
              }
            }
            case _ => ()
          }

          if( link.hasPar(cur) )
            cur = link.par(cur)
          else
            return NoBinder
        }
        assert(false); NoBinder // can't actually get here
      }
    }
  }

  case class FnApp(name: Name, args: Seq[Term], loc: Loc = NoLoc) extends Term(loc) {
    // def sort(self, info:'L4ContractTyping') -> 'Sort':
  }

  case class SortAnnotation(term: Term, sort: Sort, loc: Loc = NoLoc) extends Term(loc) {}


  abstract sealed class Literal(loc: Loc = NoLoc) extends Term(loc) {}

    case class RealLit(num: Real, loc: Loc = NoLoc) extends Literal(loc) {
      //# def sort(self, info:'L4ContractTyping') -> 'Sort':
    }

    object TimeDeltaUnit extends Enumeration {
      type TimeDeltaUnit = Value
      val s: TimeDeltaUnit = Value
      val m: TimeDeltaUnit = Value
      val h: TimeDeltaUnit = Value
      val d: TimeDeltaUnit = Value
      val w: TimeDeltaUnit = Value
    }

    case class TimeDeltaLit(num: Real, unit: TimeDeltaUnit, loc: Loc = NoLoc) extends Literal(loc) {}

