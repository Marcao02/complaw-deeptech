package miniL4.ast

import miniL4.typechecker.stdlibTyping.stdFnTypes
import miniL4.{Name, TMap, TSet}
import Statement.Block

import scala.collection.mutable

case class ContractLinking(
                       val contract: Contract,
                       val eventHandlerDefs: TMap[Name, EventHandlerDef],
                       val situationDefs: TMap[Name, SituationDef],
                       val stateVarDefs: TMap[Name, StateVarDef],
                       val parentNode: TMap[ASTNode, ASTNode],
                       val startSit: SituationDef,
                       val functions: TMap[Name, FnDec],
                       val registeredDatatypes: Set[Datatype] )
                       // not in miniL4: val datatypeAbbrevs: TMap[Name, Datatype])
  {
    def hasParentNode(node: ASTNode): Boolean = parentNode.contains(node)
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
  object Checks {
    /* Exception on failure */
    def noShadowing(linking:ContractLinking) = {
      // TODO
    }
  }

  /* create parent links, as well as links from various names to the place where the name is introduced. */
  def link(cprog: Contract): ContractLinking = {
    val eventHandlerDefs = mutable.Map[Name, EventHandlerDef]()
    val situationDefs = mutable.Map[Name, SituationDef]()
    val stateVarDefs = mutable.Map[Name, StateVarDef]()
    val parentNode = mutable.Map[ASTNode, ASTNode]()
    var startSit : Option[SituationDef] = None
    val fnDecs = stdFnTypes.name2type.map(pair => (pair._1, FnDec(pair._1, pair._2))).toMap
    var datatypes = Set.empty[Datatype]

    cprog.decs.foreach(tlnode => {
      parentNode.update(tlnode, cprog)
      tlnode match {
        case ehdef@EventHandlerDef(eventName, _, stateTransform, _, preconditions, _) => {
          for (tm <- preconditions) { linkTerm(tm, tlnode, parentNode)}
          eventHandlerDefs.update(eventName, ehdef)
          for (stmt <- stateTransform) { linkStatement(stmt, tlnode, parentNode) }
        }
        case sitdef@SituationDef(sitName, _, _, _) => {
          if (startSit.isEmpty) {
            startSit = Some(sitdef)
            println(s"Start Situation is ${sitName} since it's the first SituationDef in the list.")
          }
          situationDefs.update(sitName, sitdef)
          sitdef.eventRules.foreach(erule => {
            linkEventRule(erule, tlnode, parentNode)
          })
        }
        case svdef@StateVarDef(varName, _, _, _, _) => {
          stateVarDefs.update(varName, svdef)
        }
        case dtypeReg@RegisteredDatatypes(dtypes, _) => {
          for (dtype <- dtypes) linkDatatype(dtype, dtypeReg, parentNode)
          datatypes = dtypes
        }
      }
    })

    new ContractLinking(cprog, eventHandlerDefs, situationDefs, stateVarDefs, parentNode, startSit.get, fnDecs, datatypes)
  }

  private def linkEventRule(rule: EventRule, parent: ToplevelNode, parentNode: mutable.Map[ASTNode, ASTNode]): Unit = {
    parentNode.update(rule, parent)
    rule match {
      case InternalEventRule(eventDefName, timeTrigger, enabledGuard, ruleparamNames, paramSetter, loc) => {
        parentNode.update(timeTrigger, rule)
        enabledGuard.foreach(linkTerm(_, rule, parentNode))
        paramSetter.foreach(linkTerm(_, rule, parentNode))
      }
      case ExternalEventRule(eventDefName, roleids, timeConstraint, enabledGuard, ruleparamNames, /*paramSetter,*/ paramConstraint, loc) => {
        parentNode.update(timeConstraint, rule)
        enabledGuard.foreach(linkTerm(_, rule, parentNode))
//        paramSetter.foreach(linkTerm(_, rule, parentNode))
        paramConstraint.foreach(linkTerm(_, rule, parentNode))
      }
    }
  }

  private def linkBlock(block: Block, block_parent: ASTNode, parentNode: mutable.Map[ASTNode, ASTNode]): Unit = {
    block.foreach(stmt2 => {
      linkStatement(stmt2, block_parent, parentNode)
    })
  }

  /* Fill in `parentNode` entries for `ASTNode`s at or below stmt */
  private def linkStatement(stmt: Statement, parent: ASTNode, parentNode: mutable.Map[ASTNode, ASTNode]): Unit = {
    parentNode.update(stmt, parent)
    stmt match {
      case IfElse(test, trueBranch, falseBranch, _) => {
        linkTerm(test, stmt, parentNode)
        linkBlock(trueBranch, stmt, parentNode)
        linkBlock(falseBranch, stmt, parentNode)
      }
      case StateVarAssign(_, rhs, _) => linkTerm(rhs, stmt, parentNode)
      case LetIn(defs, block, _) => {
        defs.foreach( {case (name,term) => linkTerm(term, stmt, parentNode) } )
        linkBlock(block, stmt, parentNode)
      }
      case AssertTypeError(stmt2, _) => linkStatement(stmt2, stmt, parentNode)
    }
  }

  /* Fill in `parentNode` entries for `ASTNode`s at or below tm */
  private def linkTerm(tm: Term, parent: ASTNode, parentNode: mutable.Map[ASTNode, ASTNode]): Unit = {
    parentNode.update(tm, parent)
    tm match {
      case FnApp(fnname, args, _) => {
        for (arg <- args)
          linkTerm(arg, tm, parentNode)
      }
      case TypeAnnotation(tm2, dtype, _) => {
        linkDatatype(dtype, tm, parentNode)
        linkTerm(tm2, tm, parentNode)
      }
      case _ => ()
    }
  }

  /* Fill in `parentNode` entries for `ASTNode`s at or below dtype */
  private def linkDatatype(dtype: Datatype, parent: ASTNode, parentNode: mutable.Map[ASTNode, ASTNode]): Unit = {
    parentNode.update(dtype, parent)
    dtype match {
      case DatatypeOpApp(_, args, _) => {
        for (childDatatype <- args)
          linkDatatype(childDatatype, dtype, parentNode)
      }
      case _ => ()
    }
  }
}

