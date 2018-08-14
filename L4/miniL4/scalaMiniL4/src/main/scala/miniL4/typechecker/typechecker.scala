package miniL4.typechecker

import miniL4._
import miniL4.ast._
import miniL4.typechecker.stdlibTyping.{stdDataTypes, stdSubtypePairs}
import miniL4.typechecker.stdlibTyping.stdDataTypes.{bottomDType, boolDType, realDType, timeDeltaDType}
import astutil.{rp2hp,hp2rp,isEventHandlerParam, isEventRuleParam}
import Statement.Block

object typechecker {
  def tcassert(test:Boolean, msg: => String) : Unit = if(!test) { throw new TypeError(msg) }

  type Ctx = TMap[Name, Datatype]
  type ProofOblig = (Term, Datatype, Ctx)
  type ProofObligs = Iterable[ProofOblig]
  case class A(links: ContractLinking, graph: SubtypingGraph, ctx: Ctx)

  def typeinferTerm(tm: Term, a: A): (Datatype, ProofObligs) = tm match {
    case FnApp(name, args, loc) => {
      val arg_types_and_prf_obligs = args.map(typeinferTerm(_, a))
      val arg_types = arg_types_and_prf_obligs.view.map(_._1)
      val dtype = a.links.functions(name).fntype.rangeTypeOnArgTypes(arg_types, a.graph)
      tcassert(!dtype.isBottom, s"Problem with ${tm}")
      (dtype, arg_types_and_prf_obligs.flatMap(_._2))
    }

    case nit: NiT => (nitDatatype(nit, a.links, a.ctx), List())

    case TypeAnnotation(tm2, dtype, loc) => {
      val (inferred, proofobligs) = typeinferTerm(tm2, a)
      tcassert(subtype(dtype, inferred, a.graph), s"You can't downcast/typecast ${tm2} to ${dtype} because we can't prove that it is a subtype of ${tm2}'s inferred type ${inferred}.")
      (dtype, proofobligs ++ List((tm2, dtype, a.ctx)))
    }

    case RealLit(_, _) => (realDType, List())
    case TimeDeltaLit(_, _, _) => (timeDeltaDType, List())
    case BoolLit(_, _) => (boolDType, List())
  }

  def typecheckTerm(tm: Term, dt: Datatype, a: A): ProofObligs = {
    val (dtype, obligs) = typeinferTerm(tm, a)
    tcassert(subtype(dtype, dt, a.graph), s"Failed to prove $dtype ⊆ $dt.")
    obligs
  }

  def typecheckStatement(stmt: Statement, a: A): ProofObligs = {
    stmt match {
      case li@LetIn(defs, block, _) => {
        val def_types_proofobligs = defs.view.toMap.mapValues(rhs =>
          typeinferTerm(rhs, a))
        val def_types = proj1fromMap(def_types_proofobligs)
        val def_proofobligs = def_types_proofobligs.flatMap(_._2._2)
        typecheckBlock(block, A(a.links, a.graph, a.ctx ++ def_types)) ++ def_proofobligs
      }
      case IfElse(test, tbranch, fbranch, _) => {
        typecheckTerm(test, boolDType, a) ++
          typecheckBlock(tbranch, a) ++
          typecheckBlock(fbranch, a)
      }
      case StateVarAssign(name, rhs, _) => typecheckTerm(rhs, a.links.stateVarDefs(name).dtype, a)
      case AssertTypeError(stmt, _) => {
        try {
          typecheckStatement(stmt,a)
        } catch {
          case e:TypeError => List()
          case _:Throwable => throw new TypeError("Unexpected exception")
        }

      }
    }
  }

  def typecheckBlock(block: Block, a: A): ProofObligs = block.flatMap(typecheckStatement(_, a))

  def nitDatatype(nit: NiT, links: ContractLinking, ctx: Ctx): Datatype = {
    nit.defn(links) match {
      case LetInBinderO(src) => ctx(nit.name)
      case StateVarBinderO(src) => src.dtype
      case EventHandlerParamBinderO(eventHandlerDef) => {
//        println(s"${nit.name} is an event parameter of ${eventHandlerDef.eventName}.")
        tcassert(isEventHandlerParam(nit.name), s"${nit.name} is not a valid event handler param name. Should be nonempty and not start with '?'.")
        tcassert(ctx.contains(nit.name), s"${nit.name} should be in the typing context ${ctx}")
        ctx(nit.name)
      } // TODO feedback on error
      case EventRuleParamBinderO(_) => {
        tcassert(isEventRuleParam(nit.name), s"${nit.name} is not a valid event rule param name. Should be nonempty and start with '?'.")
        tcassert(ctx.contains(nit.name), s"${nit.name} should be in the typing context ${ctx}")
        ctx(nit.name)
      } // TODO feedback on error
      case NoBinder => throw new Exception(s"${nit.name} is unbound...")
    }
  }

  def subtype(type1: Datatype, type2: Datatype, graph: SubtypingGraph): Boolean = graph.subtype(type1, type2)

  def typecheckEventHandler(eh: EventHandlerDef, a: A): ProofObligs = {
    val a2 = A(a.links, a.graph, a.ctx ++ eh.paramsAndDatatypes)
    val from_preconds = eh.preconditions.flatMap(typecheckTerm(_, boolDType, a2))
    val from_transform = typecheckBlock(eh.stateTransform, a2)
    from_preconds ++ from_transform
  }

  def typecheckEventRule(er: EventRule, a: A): ProofObligs = {
    val from_guard = er.enabledGuard match {
      case Some(eg) => typecheckTerm(eg, boolDType, a)
      case None => List()
    }
    val from_param_constraints = er match {
      case eer@ExternalEventRule(_, _, timeConstraint, _, _, paramConstraint, _) => {
        val a2 = A(a.links, a.graph, a.ctx ++ er.paramTypePairsForRule(a.links))
        paramConstraint match {
          case Some(pc) => typecheckTerm(pc, boolDType, a2)
          case None => List()
        }
      }
      case ier@InternalEventRule(_, _, _, _, paramSetter, _) => {
        val paramTypes = ier.paramToType(a.links)
        ier.paramSetterPairs.flatMap({ case (paramname, tm) =>
          typecheckTerm(tm, paramTypes(paramname), a)
        })
      }
    }
    from_param_constraints ++ from_guard
  }

  def typecheckSituation(sit: SituationDef, a: A): ProofObligs = {
    val from_rules = sit.eventRules.flatMap(typecheckEventRule(_, a))
    val from_preconds = sit.preconditions.flatMap(typecheckTerm(_, boolDType, a))
    from_preconds ++ from_rules
  }

  /*
   Returns unit () on success, exception on failure
   */
  def typecheckContractInternal(a: A, verbose: Boolean = true): ProofObligs = {
    val from_ehs = a.links.eventHandlerDefs.values.flatMap(typecheckEventHandler(_, a))
    val from_sit = a.links.situationDefs.values.flatMap(typecheckSituation(_, a))
    from_ehs ++ from_sit
  }

  def typecheckContract(clinking:ContractLinking,
                        existingGraph:Option[SubtypingGraph] = None,
                        verbose : Boolean = true) : ProofObligs = {
    val graph = existingGraph.getOrElse(
      new SubtypingGraph(stdDataTypes.ALWAYS_INCLUDED ++ clinking.registeredDatatypes,
        stdSubtypePairs.STD_SUBTYPE_PAIRS)
    )
    typecheckContractInternal(A(clinking, graph, Map.empty[Name,Datatype]))
  }
}







//  def typecheckContract(linking:ContractLinking,
//                        existingGraph:Option[SubtypingGraph] = None,
//                        verbose : Boolean = true) : ProofObligs = {
//
//    val graph = existingGraph.getOrElse(
//      new SubtypingGraph(stdDataTypes.ALWAYS_INCLUDED ++ linking.registeredDatatypes,
//        stdSubtypePairs.STD_SUBTYPE_PAIRS)
//    )
//    val proof_obligs = mutable.Set.empty[(Term,Datatype,Ctx)]
//
//    def typeinferTerm(tm:Term, ctx:Ctx) : Datatype = tm match {
//      case FnApp(name, args, loc) => {
//        val argtypes = args.map(typeinferTerm(_,ctx))
//        linking.functions(name).fntype.rangeTypeOnArgTypes(argtypes, )
//      }
//
//      case nit:NiT => nitDatatype(nit,ctx)
//
//      case TypeAnnotation(tm2,dtype,loc) => {
//        proof_obligs.add((tm2,dtype,ctx))
//        val inferred = typeinferTerm(tm2, ctx)
//        assert(subtype(dtype, inferred), s"You can't downcast/typecast ${tm2} to ${dtype} because we can't prove that it is a subtype of ${tm2}'s inferred type ${inferred}.")
//        dtype
//      }
//
//      case RealLit(_,_) => realDType
//
//      case TimeDeltaLit(_,_,_) => timeDeltaDType
//
//      case BoolLit(_,_) => boolDType
//    }
//
//    def subtype(dtype1:Datatype, dtype2:Datatype) : Boolean = {
//      graph.hasEdge(dtype1,dtype2)
//    }
//
//    def typecheckTerm(tm:Term, dtype:Datatype, ctx:Ctx) : Boolean = {
//      val inferred = typeinferTerm(tm, ctx)
//      subtype(inferred, dtype)
//    }
//
//    def nitDatatype(nit:NiT, ctx:TMap[Name,Datatype]) : Datatype = {
//      val defn = nit.defn(linking)
//      defn match {
//        case LetInBinderO(src) => ctx(nit.name)
//        case StateVarBinderO(src) => src.dtype
//        // next two cases are wrong, since they allow referencing *any* event's params
//        case EventHandlerParamBinderO(src) => {
//          //          println("paramsAndDatatypes: ", src.paramsAndDatatypes)
//          seqmapGet(src.paramsAndDatatypes, nit.name)
//        }
//        case EventRuleParamBinderO(src) => src.getEventHandlerParamTypePair(nit.name, linking)._2
//        case NoBinder => throw new Exception("unbound name...")
//      }
//
//    }
//
//    def typecheckBlock(block:Block, ctx: Ctx = Map.empty): Ctx = {
//      var new_ctx = ctx
//      for (stmt <- block) {
//        new_ctx = typecheckStatement(stmt, new_ctx)
//      }
//      ctx // old context returned!
//    }
//
//    def typecheckStatement(stmt:Statement, __ctx: Ctx = Map.empty): Ctx = {
//      var ctx = __ctx
//      stmt match {
//        case LetIn(defs, block, _) => {
//          for((name,rhs) <- defs) {
//            val dtype = typeinferTerm(rhs, ctx)
//            ctx = ctx + (name -> dtype)
//          }
//        }
//        case IfElse(test, tbranch, fbranch, _) => {
//          typecheckTerm(test, boolDType, ctx)
//          ctx = typecheckBlock(tbranch, ctx)
//          ctx = typecheckBlock(fbranch, ctx)
//        }
//        case StateVarAssign(name, rhs, _) => {
//          typecheckTerm(rhs, linking.stateVarDefs(name).dtype, ctx)
//        }
//      }
//      ctx
//    }
//
//    for (eh <- linking.eventHandlerDefs.values) {
//
//      typecheckBlock(eh.stateTransform)
//    }
//
//    proof_obligs
//  }

