package miniL4.ast

import miniL4.{Block, Name, Real}
import miniL4.ast.Term

object astutil {
  def fnapp_helper_maker(fnsymb:Name) : (Any,Any) => Term = {
    (x,y) => {
      x match {
        case _x: Name => y match {
          case _y: Name => FnApp(fnsymb, List(NiT(_x), NiT(_y)))
          case _y: Real => FnApp(fnsymb, List(NiT(_x), RealLit(_y)))
          case _ => throw new Exception
        }
        case _x: Real => y match {
          case _y: Name => FnApp(fnsymb, List(RealLit(_x), NiT(_y)))
          case _y: Real => FnApp(fnsymb, List(RealLit(_x), RealLit(_y)))
          case _ => throw new Exception
        }
        case _ => throw new Exception
      }
    }
  }

  val leq = fnapp_helper_maker('<=)(_,_)
  val plus = fnapp_helper_maker('+)(_,_)
  val minus = fnapp_helper_maker('-)(_,_)
//  '+ -> ((x:Seq[Data]) => x(0).asInstanceOf[Real] + x(1).asInstanceOf[Real]),
//  '- -> ((x:Seq[Data]) => x(0).asInstanceOf[Real] - x(1).asInstanceOf[Real]),
//  'not -> ((x:Seq[Data]) => !x(0).asInstanceOf[Boolean])

  def forEachNodeInContract(cprog:Contract, f: ASTNode => Unit) : Unit = {
    f(cprog)
    cprog.decs.foreach(tlnode => {
      tlnode match {
        case EventHandlerDef(_, _, stateTransform, _, preconditions, _) => {
          forEachNodeInTermIter(preconditions, f)
          forEachNodeInBlock(stateTransform, f)
        }
        case SituationDef(_, eventRules, preconditions, _) => {
          for (erule <- eventRules) { forEachNodeInEventRule(erule, f) }
          forEachNodeInTermIter(preconditions, f)
        }
        case StateVarDef(_, sort, initval, _, _) => {
          forEachNodeInSort(sort, f)
          forEachNodeInTermIter(initval, f)
        }
      }
    })
  }

  def forEachNodeInEventRule(erule: EventRule, f: ASTNode => Unit): Unit = {
    f(erule)
    erule match {
      case InternalEventRule(eventDefName, timeTrigger, enabledGuard, ruleparamNames, paramSetter, loc) => {
        f(timeTrigger)
        forEachNodeInTermIter(enabledGuard, f)
        forEachNodeInTermIter(paramSetter, f)
      }
      case ExternalEventRule(eventDefName, roleids, timeConstraint, enabledGuard, ruleparamNames, /*paramSetter,*/ paramConstraint, loc) => {
        f(timeConstraint)
        forEachNodeInTermIter(enabledGuard, f)
//        forEachNodeInTermIter(paramSetter, f)
        forEachNodeInTermIter(paramConstraint, f)
      }
    }
  }

  def forEachNodeInBlock(block: Block, f: ASTNode => Unit): Unit = block.foreach(forEachNodeInStatement(_, f))
  def forEachNodeInTermIter(seq: Iterable[Term], f: ASTNode => Unit): Unit = seq.foreach(forEachNodeInTerm(_, f))

  def forEachNodeInStatement(stmt: Statement, f: ASTNode => Unit) : Unit = {
    f(stmt)
    stmt match {
      case IfElse(test, trueBranch, falseBranch, _) => {
        forEachNodeInTerm(test, f)
        forEachNodeInBlock(trueBranch, f)
        forEachNodeInBlock(falseBranch, f)
      }
      case StateVarAssign(_, rhs, _) => {
        forEachNodeInTerm(rhs, f)
      }
      case LetIn(defs, block, _) => {
        defs.foreach({case (name,term) => {
          forEachNodeInTerm(term, f)
        }})
      }
    }
  }

  def forEachNodeInTerm(tm: Term, f: ASTNode => Unit) : Unit = {
    f(tm)
    tm match {
      case FnApp(fnname, args, _) => forEachNodeInTermIter(args,f)
      case SortAnnotation(tm2, sort, _) => {
        forEachNodeInSort(sort, f)
        forEachNodeInTerm(tm2, f)
      }
      case _ => ()
    }
  }

  def forEachNodeInSort(sort: Sort, f: ASTNode => Unit) : Unit = {
    f(sort)
    sort match {
      case SortOpApp(_, args, _) => {
        for (childSort <- args) forEachNodeInSort(childSort, f)
      }
      case _ => ()
    }
  }
}
