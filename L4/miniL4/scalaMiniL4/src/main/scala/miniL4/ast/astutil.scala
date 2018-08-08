package miniL4.ast

import miniL4.{Block, Name, Real}

object astutil {
  def real_fnapp_helper_maker(fnsymb:Name) : (Any,Any) => Term = {
    (x,y) => {
      x match {
        case _x: Name => y match {
          case _y: Name => FnApp(fnsymb, List(NiT(_x), NiT(_y)))
          case _y: Real => FnApp(fnsymb, List(NiT(_x), RealLit(_y)))
          case _y: Int => FnApp(fnsymb, List(NiT(_x), RealLit(_y)))
          case _ => throw new Exception
        }
        case _x: Real => y match {
          case _y: Name => FnApp(fnsymb, List(RealLit(_x), NiT(_y)))
          case _y: Real => FnApp(fnsymb, List(RealLit(_x), RealLit(_y)))
          case _y: Int => FnApp(fnsymb, List(RealLit(_x), RealLit(_y)))
          case _ => throw new Exception
        }
        case _ => throw new Exception
      }
    }
  }
  def bool_fnapp_helper_maker(fnsymb:Name) : (Any,Any) => Term = {
    (x,y) => {
      x match {
        case _x: Name => y match {
          case _y: Name => FnApp(fnsymb, List(NiT(_x), NiT(_y)))
          case _y: Term => FnApp(fnsymb, List(NiT(_x), _y))
          case _ => throw new Exception
        }
        case _x: Term => y match {
          case _y: Name => FnApp(fnsymb, List(_x, NiT(_y)))
          case _y: Term => FnApp(fnsymb, List(_x, _y))
          case _ => throw new Exception
        }
        case _ => throw new Exception
      }
    }
  }


  val leq = real_fnapp_helper_maker('<=)(_,_)
  val geq = real_fnapp_helper_maker('>=)(_,_)
  val plus = real_fnapp_helper_maker('+)(_,_)
  val minus = real_fnapp_helper_maker('-)(_,_)
  val and = bool_fnapp_helper_maker('and)(_,_)
  val or = bool_fnapp_helper_maker('or)(_,_)
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
        case StateVarDef(_, dtype, initval, _, _) => {
          forEachNodeInDatatype(dtype, f)
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
      case TypeAnnotation(tm2, dtype, _) => {
        forEachNodeInDatatype(dtype, f)
        forEachNodeInTerm(tm2, f)
      }
      case _ => ()
    }
  }

  def forEachNodeInDatatype(dtype: Datatype, f: ASTNode => Unit) : Unit = {
    f(dtype)
    dtype match {
      case DatatypeOpApp(_, args, _) => {
        for (childDtype <- args) forEachNodeInDatatype(childDtype, f)
      }
      case _ => ()
    }
  }
}
