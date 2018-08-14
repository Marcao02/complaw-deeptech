package miniL4.ast

import miniL4.{Name, Real}
import Statement.Block

object astutil {
  def lit(x:Real) : RealLit = RealLit(x)
  def lit(x:Int) : RealLit = RealLit(x)
  def lit(x:Boolean) : BoolLit = BoolLit(x)


  def fnapp_helper_maker(fnsymb:Name) : List[Any] => Term = args => {
    val args_wrapped = args.map( arg => arg match {
        case _arg: Name => NiT(_arg)
        case _arg: Term => _arg
        case _arg: Real => RealLit(_arg)
        case _arg: Int => RealLit(_arg)
        case _arg: Boolean => BoolLit(_arg)
      }
    )
    FnApp(fnsymb, args_wrapped)
  }
  def fnapp_helper_maker1(fnsymb:Name) : Any => Term = x => fnapp_helper_maker(fnsymb)(List(x))
  def fnapp_helper_maker2(fnsymb:Name) : (Any,Any) => Term = (x,y) => fnapp_helper_maker(fnsymb)(List(x,y))

  val leq = fnapp_helper_maker2('<=)(_,_)
  val geq = fnapp_helper_maker2('>=)(_,_)
  val plus = fnapp_helper_maker2('+)(_,_)
  val minus = fnapp_helper_maker2('-)(_,_)
  val and = fnapp_helper_maker2('and)(_,_)
  val or = fnapp_helper_maker2('or)(_,_)
  val not = fnapp_helper_maker1('not)(_)
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
        case RegisteredDatatypes(dtypes, _) => {
          for (dtype <- dtypes) forEachNodeInDatatype(dtype, f)
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
      case AssertTypeError(stmt2, _) => forEachNodeInStatement(stmt2, f)
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
