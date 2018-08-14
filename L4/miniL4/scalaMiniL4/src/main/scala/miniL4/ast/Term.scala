package miniL4.ast

import TimeDeltaUnit.TimeDeltaUnit
import miniL4.{Name, Real, seqmapHasKey}

// NOTE that these definitions aren't actually nested. It's just conceptual nesting.

abstract sealed class Term(loc: Loc) extends ASTNode(loc) {}

  /* Name in Term */
  case class NiT(name: Name, loc: Loc = NoLoc) extends Term(loc) {
//    def datatype(self, typing: ContractTyping) : Datatype = {
//      val defnOpt = this.defn(linking)
//      defnOpt match {
//        case LetInBinderO => {
//
//        }
//        case NoBinder => throw new Exception(s"Name ${name} at ${loc} does not dereference to any TermBinder.")
//      }
//    }

    def defn(linking:ContractLinking) : TermBinderO = {
      if( linking.stateVarDefs.contains(this.name) )
        StateVarBinderO(linking.stateVarDefs(this.name))
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
            case ehd@EventHandlerDef(_, _, _, paramsAndDatatypes, _, _) => {
              if(seqmapHasKey(paramsAndDatatypes, this.name))
                return EventHandlerParamBinderO(ehd)
            }
            case er:EventRule => {
              if(er.ruleparamNames.contains(this.name))
                return EventRuleParamBinderO(er)
            }
            case _ => ()
          }

          if( linking.hasParentNode(cur) )
            cur = linking.parentNode(cur)
          else
            return NoBinder
        }
        assert(false); NoBinder // can't actually get here
      }
    }
  }

  case class FnApp(name: Name, args: List[Term], loc: Loc = NoLoc) extends Term(loc) {
    // def datatype(self, info:'L4ContractTyping') -> 'Datatype':
  }

  case class TypeAnnotation(term: Term, dtype: Datatype, loc: Loc = NoLoc) extends Term(loc) {}

  // TODO: <term> as <datatype> or ... or <datatype> in <block>
  // TODO: type cases <term> (<datatype>, <block>) ... (<datatype>, <block>)

  abstract sealed class Literal(loc: Loc = NoLoc) extends Term(loc) {}

    case class RealLit(num: Real, loc: Loc = NoLoc) extends Literal(loc) {
      //# def datatype(self, info:'L4ContractTyping') -> 'Datatype':
    }

    case class BoolLit(bool: Boolean, loc: Loc = NoLoc) extends Literal(loc) {}

    object TimeDeltaUnit extends Enumeration {
      type TimeDeltaUnit = Value
      val s: TimeDeltaUnit = Value
      val m: TimeDeltaUnit = Value
      val h: TimeDeltaUnit = Value
      val d: TimeDeltaUnit = Value
      val w: TimeDeltaUnit = Value
    }

    case class TimeDeltaLit(num: Real, unit: TimeDeltaUnit, loc: Loc = NoLoc) extends Literal(loc) {}

