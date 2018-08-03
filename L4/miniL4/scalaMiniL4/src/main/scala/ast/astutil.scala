package ast

object astutil {
  def forEachNodeInContract(cprog:Contract, f: ASTNode => Unit) : Unit = {
    f(cprog)
    cprog.decs.foreach(tlnode => {
      tlnode match {
        case EventHandlerDef(_, _, stateTransform, preconditions, _) => {
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
      case InternalEventRule(eventDefName, timeTrigger, entranceGuard, ruleparamNames, paramSetter, loc) => {
        f(timeTrigger)
        forEachNodeInTermIter(entranceGuard, f)
        forEachNodeInTermIter(paramSetter, f)
      }
      case ExternalEventRule(eventDefName, roleids, timeConstraint, entranceGuard, ruleparamNames, paramSetter, paramConstraint, loc) => {
        f(timeConstraint)
        forEachNodeInTermIter(entranceGuard, f)
        forEachNodeInTermIter(paramSetter, f)
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
