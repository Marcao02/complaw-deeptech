package ast

import scala.collection.mutable

class ContractLinking(
  val contract: Contract,
  val eventHandlerDefs: IMap[Name, EventHandlerDef],
  val situationDefs: IMap[Name, SituationDef],
  val stateVarDefs: IMap[Name, StateVarDef],
  val par: IMap[ASTNode, ASTNode] )
  {
    def hasPar(node: ASTNode): Boolean = par.contains(node)
  }

object ContractLinking {

  /* create parent links, as well as links from various names to the place where the name is introduced. */
  def link(cprog: Contract): ContractLinking = {
    val eventHandlerDefs = mutable.Map[Name, EventHandlerDef]()
    val situationDefs = mutable.Map[Name, SituationDef]()
    val stateVarDefs = mutable.Map[Name, StateVarDef]()
    val par = mutable.Map[ASTNode, ASTNode]()

    cprog.decs.foreach(tlnode => {
      par.update(tlnode, cprog)
      tlnode match {
        case EventHandlerDef(eventName, _, _, _) => {
          // these `asInstanceOf`s shouldn't be necessary...
          eventHandlerDefs.update(eventName, tlnode.asInstanceOf[EventHandlerDef])
          tlnode.asInstanceOf[EventHandlerDef].stateTransform.foreach(stmt => {
            linkStatement(stmt, tlnode, par)
          })
        }
        case SituationDef(sitName, _, _, _) => {
          situationDefs.update(sitName, tlnode.asInstanceOf[SituationDef])
          tlnode.asInstanceOf[SituationDef].eventRules.foreach(erule => {
            linkEventRule(erule, tlnode, par)
          })
        }
        case StateVarDef(varName, _, _, _, _) => {
          stateVarDefs.update(varName, tlnode.asInstanceOf[StateVarDef])
        }
      }
    })

    new ContractLinking(cprog, eventHandlerDefs, situationDefs, stateVarDefs, par)
  }

  private def linkEventRule(rule: EventRule, parent: ToplevelNode, par: mutable.Map[ASTNode, ASTNode]): Unit = {
    par.update(rule, parent)
    rule match {
      case InternalEventRule(eventDefName, timeTrigger, entranceGuard, ruleparamNames, paramSetter, loc) => {
        par.update(timeTrigger, rule)
        entranceGuard.foreach(linkTerm(_, rule, par))
        paramSetter.foreach(linkTerm(_, rule, par))
      }
      case ExternalEventRule(eventDefName, roleids, timeConstraint, entranceGuard, ruleparamNames, paramSetter, paramConstraint, loc) => {
        par.update(timeConstraint, rule)
        entranceGuard.foreach(linkTerm(_, rule, par))
        paramSetter.foreach(linkTerm(_, rule, par))
        paramConstraint.foreach(linkTerm(_, rule, par))
      }
    }
  }

  private def linkBlock(block: Block, block_parent: ASTNode, par: mutable.Map[ASTNode, ASTNode]): Unit = {
    block.foreach(stmt2 => {
      linkStatement(stmt2, block_parent, par)
    })
  }

  /* Fill in `par` entries for `ASTNode`s at or below stmt */
  private def linkStatement(stmt: Statement, parent: ASTNode, par: mutable.Map[ASTNode, ASTNode]): Unit = {
    par.update(stmt, parent)
    stmt match {
      case IfElse(test, trueBranch, falseBranch, _) => {
        linkTerm(test, stmt, par)
        linkBlock(trueBranch, stmt, par)
        linkBlock(falseBranch, stmt, par)
      }
      case StateVarAssign(_, rhs, _) => linkTerm(rhs, stmt, par)
      case LetIn(defs, block, _) => {
        defs.foreach( {case (name,term) => linkTerm(term, stmt, par) } )
        linkBlock(block, stmt, par)
      }
    }
  }

  /* Fill in `par` entries for `ASTNode`s at or below tm */
  private def linkTerm(tm: Term, parent: ASTNode, par: mutable.Map[ASTNode, ASTNode]): Unit = {
    par.update(tm, parent)
    tm match {
      case FnApp(fnname, args, _) => {
        for (arg <- args)
          linkTerm(arg, tm, par)
      }
      case SortAnnotation(tm2, sort, _) => {
        linkSort(sort, tm, par)
        linkTerm(tm2, tm, par)
      }
      case _ => ()
    }
  }

  /* Fill in `par` entries for `ASTNode`s at or below sort */
  private def linkSort(sort: Sort, parent: ASTNode, par: mutable.Map[ASTNode, ASTNode]): Unit = {
    par.update(sort, parent)
    sort match {
      case SortOpApp(_, args, _) => {
        for (childSort <- args)
          linkSort(childSort, sort, par)
      }
      case _ => ()
    }
  }
}

