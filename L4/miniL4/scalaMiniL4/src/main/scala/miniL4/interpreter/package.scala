import miniL4._
import ast.{astutil, _}
import miniL4.ast.time._
import miniL4.interpreter.{EvalCtx, L4Event}
import scalax.collection.GraphEdge.DiEdge
import scalax.collection.GraphPredef._
import scalax.collection.mutable.{Graph => MutDiGraph}

package object interpreter {
  val fnInterps : TMap[Name, Seq[Data] => Data]= Map(
    '+ -> ((x:Seq[Data]) => x(0).asInstanceOf[Real] + x(1).asInstanceOf[Real]),
    'not -> ((x:Seq[Data]) => !x(0).asInstanceOf[Boolean])
  )

  type NameDiGraph = scalax.collection.mutable.Graph[Name,DiEdgeLikeIn]
  type Data = Any
  type Trace = Seq[L4Event]

  // turn occurs-in relation (state var occurs in the right hand side of another state var's assignment statement) into a graph
  def assignSetToGraph(svAssigns: TMap[Name,StateVarAssign], clink:ContractLinking) : NameDiGraph = {
    val graph = MutDiGraph[Name,DiEdgeLikeIn]()

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

    var ctx = EvalCtx(Map.empty, clink.stateVarDefs.keySet, Map.empty, Map.empty, clink.startSit)
    ctx = evalStateVarAssigns(svas, ctx, clink)

    var prev_ts = 0

    trace.foreach( event => {
      val next_ts = event.timeStamp

      // let's evaluate all the event rules, since if more than one is triggered that's something we
      // ought to tell the user about
      if(event.roleName == CONTRACT_ROLE) {
        // todo: evaluate clink.internalEventRules(ctx.cur_sit)

        // no exception
      }
      else {
        val for_this_role = clink.externalEventRules(ctx.cur_sit).filter(erule => erule.roleIds.contains(event.roleName))
        assert(for_this_role.nonEmpty, s"No event rules for role ${event.roleName} in situation ${ctx.cur_sit}.")

        val for_this_role_and_event = for_this_role.filter(erule => erule.eventDefName == event.eventName)
        assert(for_this_role.nonEmpty,
          s"Among the ${for_this_role.size} event rules for role ${event.roleName} in situation ${ctx.cur_sit}, none allow doing ${event.eventName} ")

        val enabled_for_this_role_and_event = for_this_role_and_event.filter(
          erule => erule.enabledGuard.isEmpty || evalTerm(erule.enabledGuard.get, ctx, clink) == true )
        assert(enabled_for_this_role_and_event.nonEmpty,
          s"Among the ${for_this_role_and_event} event rules allowing role ${event.roleName} to do ${event.eventName} in situation ${ctx.cur_sit}, none was enabled upon entering the present Situation." )

        val final_rules = enabled_for_this_role_and_event.filter(
          erule => {
            val ctx_for_rule_eval = ctx.withEventParamsUpdated(
              clink.eventHandlerDefs(erule.eventDefName).params.zip( event.params ).toMap
            )
            ((erule.paramConstraint.isEmpty || evalTerm(erule.paramConstraint.get, ctx_for_rule_eval, clink) == true )
            & (evalTimeConstraint(erule.timeConstraint, (prev_ts,next_ts), ctx_for_rule_eval, clink) == true))
          })
        assert(final_rules.nonEmpty,
          s"Among the enabled ${enabled_for_this_role_and_event} event rules allowing role ${event.roleName} to do ${event.eventName} in situation ${ctx.cur_sit}, none has both its event parameter constraint and time constraint satisfied.")

        warn(final_rules.size > 1, "Multiple rules apply. Execution is unambiguous, but perhaps you didn't intend this?")

        // no exception
      }

      ctx = evalEvent(event, ctx, clink)
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

  def evalStateVarAssigns(svAssigns: TMap[Name,StateVarAssign], _ctx: EvalCtx, clink: ContractLinking) : EvalCtx = {
    // turn occurs-in relation into a graph
    val graph = assignSetToGraph(svAssigns, clink)
    var ctx = _ctx
    graph.topologicalSort.fold(
      (cycleNode:graph.NodeT) => assert(false,s"Cycle involving ${cycleNode}"),
      order => {
        order.foreach(node => {
          val svname = node.asInstanceOf[Name]
          val svval = evalTerm(svAssigns(svname).rhs, ctx, clink)
          ctx = EvalCtx(ctx.sv_vals.updated(svname, svval), ctx.sv_uninit - svname, ctx.locv_vals, ctx.eparam_vals, ctx.cur_sit)
        })
      }
    )
    ctx
  }

  def evalEvent(event: L4Event, ctx: EvalCtx, clink: ContractLinking) : EvalCtx = {
    evalEventHandler(clink.eventHandlerDefs(event.eventName), ctx, clink)
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
            _stmts = _stmts - stmt
            if(testval)
              _stmts = _stmts ++ tbranch
            else
              _stmts = _stmts ++ fbranch
            progress = true
          }
          case StateVarAssign(_,_,_) => ()
        }
      }
    }
    assert( _stmts.forall(_.isInstanceOf[StateVarAssign]) )

    val svAssigns = (_stmts.asInstanceOf[TSet[StateVarAssign]].map( sva => (sva.name, sva) )).toMap
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
          case NoBinder => assert(false, "error msg toto")
        }
      }
      case FnApp(fnname, args, _) => {
        val argvals = args.map( evalTerm(_, ctx, clink) )
        fnInterps(fnname)(argvals)
      }
      case SortAnnotation(tm, sort, _) => {
        val tmval = evalTerm(tm, ctx, clink)
        // TODO: check inclusion in sort.
        // doing it just for Real:
        if(tmval.isInstanceOf[Real]) {
          assert(sort == AtomicSort('Real))
        }
        tmval
      }
    }
  }
}
