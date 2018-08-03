package miniL4.ast

import miniL4.{Block, Name, TMap, TSet}

import scala.collection.mutable

case class ContractLinking(
                       val contract: Contract,
                       val eventHandlerDefs: TMap[Name, EventHandlerDef],
                       val situationDefs: TMap[Name, SituationDef],
                       val stateVarDefs: TMap[Name, StateVarDef],
                       val par: TMap[ASTNode, ASTNode],
                       val startSit: SituationDef )
  {
    def hasPar(node: ASTNode): Boolean = par.contains(node)
    val externalEventRules : TMap[SituationDef, TSet[ExternalEventRule]] = situationDefs.values.map( sitdef => {
      (sitdef, sitdef.eventRules.filter(_.isInstanceOf[ExternalEventRule]).toSet.asInstanceOf[TSet[ExternalEventRule]])
    }).toMap
    val internalEventRules : TMap[SituationDef, TSet[InternalEventRule]] = situationDefs.values.map( sitdef => {
      (sitdef, sitdef.eventRules.filter(_.isInstanceOf[InternalEventRule]).toSet.asInstanceOf[TSet[InternalEventRule]])
    }).toMap
//    val externalEventRules : TMap[SituationDef, TSet[ExternalEventRule]] = situationDefs.mapValues( sitdef => {
//      sitdef.eventRules.filter(_.isInstanceOf[ExternalEventRule]).toSet
//    })
//    val internalEventRules : TMap[SituationDef, TSet[InternalEventRule]] = situationDefs.mapValues( sitdef => {
//      sitdef.eventRules.filter(_.isInstanceOf[InternalEventRule]).toSet
//    })
  }

object ContractLinking {
  /* create parent links, as well as links from various names to the place where the name is introduced. */
  def link(cprog: Contract): ContractLinking = {
    val eventHandlerDefs = mutable.Map[Name, EventHandlerDef]()
    val situationDefs = mutable.Map[Name, SituationDef]()
    val stateVarDefs = mutable.Map[Name, StateVarDef]()
    val par = mutable.Map[ASTNode, ASTNode]()

    var startSit : Option[SituationDef] = None

    cprog.decs.foreach(tlnode => {
      par.update(tlnode, cprog)
      tlnode match {
        case ehdef@EventHandlerDef(eventName, _, stateTransform, _, preconditions, _) => {
          for (tm <- preconditions) { linkTerm(tm, tlnode, par)}
          eventHandlerDefs.update(eventName, ehdef)
          for (stmt <- stateTransform) { linkStatement(stmt, tlnode, par) }
        }
        case sitdef@SituationDef(sitName, _, _, _) => {
          if (startSit.isEmpty) {
            startSit = Some(sitdef)
            println(s"Start Situation is ${sitName} since it's the first SituationDef in the list.")
          }
          situationDefs.update(sitName, sitdef)
          sitdef.eventRules.foreach(erule => {
            linkEventRule(erule, tlnode, par)
          })
        }
        case svdef@StateVarDef(varName, _, _, _, _) => {
          stateVarDefs.update(varName, svdef)
        }
      }
    })

    new ContractLinking(cprog, eventHandlerDefs, situationDefs, stateVarDefs, par, startSit.get)
  }

  private def linkEventRule(rule: EventRule, parent: ToplevelNode, par: mutable.Map[ASTNode, ASTNode]): Unit = {
    par.update(rule, parent)
    rule match {
      case InternalEventRule(eventDefName, timeTrigger, enabledGuard, ruleparamNames, paramSetter, loc) => {
        par.update(timeTrigger, rule)
        enabledGuard.foreach(linkTerm(_, rule, par))
        paramSetter.foreach(linkTerm(_, rule, par))
      }
      case ExternalEventRule(eventDefName, roleids, timeConstraint, enabledGuard, ruleparamNames, /*paramSetter,*/ paramConstraint, loc) => {
        par.update(timeConstraint, rule)
        enabledGuard.foreach(linkTerm(_, rule, par))
//        paramSetter.foreach(linkTerm(_, rule, par))
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

