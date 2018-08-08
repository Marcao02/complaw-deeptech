package miniL4.interpreter

import interpreter.fnInterps
import miniL4.ast.time._
import miniL4.interpreter.Trace.Trace
import miniL4._
import scalax.collection.GraphPredef.DiEdgeLikeIn
import ast.{NoBinder, astutil, _}
//import interpreter.Data
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.mutable.{Graph => MutDiGraph}

object evalL4 {
  type Data = Any

  type NameDiGraph = scalax.collection.mutable.Graph[Name,DiEdgeLikeIn]

  // turn occurs-in relation (state var occurs in the right hand side of another state var's assignment statement) into a graph
  def assignSetToGraph(svAssigns: TMap[Name,StateVarAssign], clink:ContractLinking) : NameDiGraph = {
    val graph = MutDiGraph[Name,DiEdgeLikeIn]()

    svAssigns.foreach({ case (svname, _) =>
      graph.add(svname)
    })

    svAssigns.foreach({ case (parent_name, sva) => {
      def f(node:ASTNode): Unit = node match {
        case sv_nit:NiT => {
          sv_nit.defn(clink) match {
            case StateVarBinderO(svd) => {
              graph.add(DiEdge(svd.name, parent_name))
            }
            case _ => ()
          }
        }
        case _ => ()
      }
      astutil.forEachNodeInTerm(sva.rhs, f)
    }})

    graph
  }

  def evalTrace(trace:Trace, clink:ContractLinking) : Unit = {
    // view the state var defs with initial vals as StateVarAssign's
    val svas = clink.stateVarDefs.filter({ case (name,svd) => {  svd.initVal.nonEmpty } })
      .mapValues( svd => { StateVarAssign(svd.name, svd.initVal.get, svd.loc) } )

    //    println(s"State var defs: ", clink.stateVarDefs.keySet)
    var ctx = EvalCtx(Map.empty, clink.stateVarDefs.keySet, Map.empty, Map.empty, clink.startSit)
    ctx = evalStateVarAssigns(svas, ctx, clink)
    //    println(s"Initial state var vals: ", ctx.sv_vals)

    var prev_ts = 0

    trace.foreach( event => {
      val next_ts = event.timeStamp

      // let's evaluate all the event rules, since if more than one is triggered that's something we
      // ought to tell the user about
      if(event.roleName == CONTRACT_ROLE) {
        val for_this_role = clink.internalEventRules(ctx.cur_sit)
        assert(for_this_role.nonEmpty, s"\nNo internal event rules in situation ${ctx.cur_sit.name}.\n")

        val for_this_role_and_event = for_this_role.filter(erule => erule.eventDefName == event.eventName)
        assert(for_this_role_and_event.nonEmpty,
          s"\nNo internal event rules for ${event.eventName} in situation ${ctx.cur_sit.name}.\n")

        val enabled_for_this_role_and_event = for_this_role_and_event.filter(
          erule => erule.enabledGuard.isEmpty || evalTerm(erule.enabledGuard.get, ctx, clink) == true )
        assert(enabled_for_this_role_and_event.nonEmpty,
          s"\nAmong the ${for_this_role_and_event.size} internal event rule(s):\n${EventRule.minimalEventRuleCollToString(for_this_role_and_event)}\nfor ${event.eventName} in situation ${ctx.cur_sit.name}, none was enabled upon entering the present Situation.\n" )

        val final_rules = enabled_for_this_role_and_event.filter(
          erule => {
            val paramsOk = erule.paramSetter.isEmpty || erule.paramSetterMap().forall({case (name,term) =>
              event.paramVals(name) == term})
            val ctx_for_rule_eval = ctx.withEventParamsUpdated(event.paramVals)
            val timeOk = evalTimeTrigger(erule.timeTrigger, prev_ts, ctx_for_rule_eval, clink) == next_ts
            paramsOk && timeOk
          })

        assert(final_rules.nonEmpty,
          s"\nAmong the ${enabled_for_this_role_and_event.size} enabled internal event rule(s):\n${EventRule.minimalEventRuleCollToString(enabled_for_this_role_and_event)}\nfor ${event.eventName} in situation ${ctx.cur_sit.name}, none give the correct values for the event parameters and timestamp specified by the event.\n")

        warn(final_rules.size > 1, s"Multiple rules apply. Execution is unambiguous, but perhaps you didn't intend this? These are the rules:\n${EventRule.minimalEventRuleCollToString(final_rules)}\n")
        // no exception
      }
      else {
        val for_this_role = clink.externalEventRules(ctx.cur_sit).filter(erule => erule.roleIds.contains(event.roleName))
        assert(for_this_role.nonEmpty, s"\nNo event rules for role ${event.roleName} in situation ${ctx.cur_sit.name}.")

        val for_this_role_and_event = for_this_role.filter(erule => erule.eventDefName == event.eventName)
        assert(for_this_role_and_event.nonEmpty,
          s"\nAmong the ${for_this_role.size} event rule(s):\n${EventRule.minimalEventRuleCollToString(for_this_role)}\nfor role ${event.roleName} in situation ${ctx.cur_sit.name}, none allow doing ${event.eventName}.\n")

        val enabled_for_this_role_and_event = for_this_role_and_event.filter(
          erule => erule.enabledGuard.isEmpty || evalTerm(erule.enabledGuard.get, ctx, clink) == true )
        assert(enabled_for_this_role_and_event.nonEmpty,
          s"\nAmong the ${for_this_role_and_event.size} event rule(s):\n${EventRule.minimalEventRuleCollToString(for_this_role_and_event)}\nallowing role ${event.roleName} to do ${event.eventName} in situation ${ctx.cur_sit.name}, none was enabled upon entering the present Situation.\n" )

        val final_rules = enabled_for_this_role_and_event.filter(
          erule => {
            val ctx_for_rule_eval = ctx.withEventParamsUpdated(
              event.paramVals
            )
            val paramsOk = erule.paramConstraint.isEmpty || evalTerm(erule.paramConstraint.get, ctx_for_rule_eval, clink) == true
            val timeConstraintOk = evalTimeConstraint(erule.timeConstraint, (prev_ts,next_ts), ctx_for_rule_eval, clink)
            paramsOk && timeConstraintOk
          })

        assert(final_rules.nonEmpty,
          s"\nAmong the ${enabled_for_this_role_and_event.size} enabled event rule(s):\n${EventRule.minimalEventRuleCollToString(enabled_for_this_role_and_event)}\nallowing role ${event.roleName} to do ${event.eventName} in situation ${ctx.cur_sit.name}, none has both its event parameter constraint and time constraint satisfied.\n")

        warn(final_rules.size > 1, s"Multiple rules apply. Execution is unambiguous, but perhaps you didn't intend this? These are the rules:\n${EventRule.minimalEventRuleCollToString(final_rules)}\n")

        // no exception
      }

      ctx = evalEvent(event, ctx, clink)

      //      println(s"Done with ${event.eventName}. Now in ${ctx.cur_sit.name}.")
    })
  }

  def evalTimeConstraint(tc:TimeConstraint, prev_next_ts:(Real,Real), ctx:EvalCtx, clink:ContractLinking) : Boolean = {
    val (prev_ts, next_ts) = prev_next_ts
    tc match {
      case NoTimeConstraint(_) => true
      case BeforeTimeDeltaFromStart(td,_) => next_ts < td.num
      case WithinTimeDeltaFromStart(td,_) => next_ts <= td.num
      case BeforeTimeDeltaSplit(td,_) => next_ts < (td.num + prev_ts)
      case WithinTimeDeltaSplit(td,_) => next_ts <= (td.num + prev_ts)
    }
  }

  def evalTimeTrigger(tc:TimeTrigger, prev_ts:Real, ctx:EvalCtx, clink:ContractLinking) : Real = {
    tc match {
      case AtTimeDelta(td,_) => td.num
      case AfterTimeDelta(td,_) => td.num + 1
    }
  }

  def evalStateVarAssigns(svAssigns: TMap[Name,StateVarAssign], _ctx: EvalCtx, clink: ContractLinking) : EvalCtx = {
    // turn occurs-in relation into a graph
    val graph = assignSetToGraph(svAssigns, clink)
    var ctx = _ctx
    //    println("The graph: ", graph)
    graph.topologicalSort.fold(
      (cycleNode:graph.NodeT) => assert(false,s"Cycle involving ${cycleNode}"),
      order => {
        //        println("The topological order: ", order)
        order.foreach(node => {
          val svname = node.value
          val svval = evalTerm(svAssigns(svname).rhs, ctx, clink)
          //          println(s"Initial val of ${svname} is ${svval}.")
          ctx = EvalCtx(ctx.sv_vals.updated(svname, svval), ctx.sv_uninit - svname, ctx.locv_vals, ctx.eparam_vals, ctx.cur_sit)
        })
      }
    )
    ctx
  }

  def evalEvent(event: L4Event, ctx: EvalCtx, clink: ContractLinking) : EvalCtx = {
    val eh = clink.eventHandlerDefs(event.eventName)
//    ctx2 = ctx.withEventParamsUpdated(eh.params.zip(event.params).toMap)
    val ctx2 = ctx.withEventParamsUpdated(event.paramVals)
    evalEventHandler(eh, ctx2, clink).withCurSitUpdated(clink.situationDefs(eh.destSit))
  }

  def evalEventHandler(eh: EventHandlerDef, ctx: EvalCtx, clink: ContractLinking) : EvalCtx = {
    for (pre <- eh.preconditions) { assert( evalTerm(pre, ctx, clink) == true, "precondition failed" )}
    evalBlock(eh.stateTransform, ctx, clink)
  }

  def evalBlock(stmts: Block, ctx: EvalCtx, clink: ContractLinking) : EvalCtx = {
    var _ctx = ctx
    var progress = true

    // eliminate (in worst case quadratic time in |stmts|, but doesn't matter) IfElse and LetIn
    var _stmts = Set() ++ stmts
    while(progress) {
      progress = false
      for (stmt <- _stmts) {
        stmt match {
          case LetIn(defs, block, _) => {
            _ctx = _ctx.withLocalVarsUpdated( defs.map({case (name,rhs) => (name, evalTerm(rhs,ctx,clink))}).toMap )
            _stmts = _stmts - stmt ++ block
            progress = true
          }
          case IfElse(test, tbranch, fbranch, _) => {
            val testval = evalTerm(test,ctx,clink).asInstanceOf[Boolean]
            _stmts = _stmts - stmt ++ (if(testval) tbranch else fbranch)
            progress = true
          }
          case StateVarAssign(_,_,_) => ()
        }
      }
    }
    assert( _stmts.forall(_.isInstanceOf[StateVarAssign]) )

    val svAssigns = _stmts.asInstanceOf[TSet[StateVarAssign]].map(sva => (sva.name, sva)).toMap
    evalStateVarAssigns( svAssigns, _ctx, clink )
  }

  def evalTerm(term:Term, ctx:EvalCtx, clink:ContractLinking) : Data = {
    term match {
      case RealLit(x,_) => x
      case TimeDeltaLit(x,_,_) => x // WARN: I'm ignoring units, since this is miniL4
      case nit:NiT => {
        nit.defn(clink) match {
          case LetInBinderO(_) => ctx.locv_vals(nit.name)
          case StateVarBinderO(_) => ctx.sv_vals(nit.name)
          case EventHandlerParamBinderO(_) => ctx.eparam_vals(nit.name)
          case EventRuleParamBinderO(_) => ctx.eparam_vals(nit.name)
          case NoBinder => assert(false, "can't happen")
        }
      }
      case FnApp(fnname, args, _) => {
        val argvals = args.map( evalTerm(_, ctx, clink) )
        assert(fnInterps.contains(fnname), s"Don't know how to interpret function symbol $fnname")
        fnInterps(fnname)(argvals)
      }
      case TypeAnnotation(tm, dtype, _) => {
        val tmval = evalTerm(tm, ctx, clink)
        // doing it just for Real and Boolean, since those are the only atomic types in miniL4
        tmval match {
          case _: Real => assert(dtype == AtomicDatatype('Real) || dtype == AtomicDatatype('TimeDelta))
          case _: Boolean => assert(dtype == AtomicDatatype('Bool))
        }
        tmval
      }
    }
  }
}
