import ast.astutil
import ast._
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
  import ast.{Name, Real}
  case class Event(eventName: Name, roleName: Name, timeStamp: Real, params: Seq[Data]) { }
  type Trace = Seq[Event]

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

    var ctx = EvalCtx(Map.empty, clink.stateVarDefs.keySet, Map.empty, Map.empty)
    ctx = evalStateVarAssigns(svas, ctx, clink)

    trace.foreach( event => {
      ctx = evalEvent(event, ctx, clink)
    })
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
          ctx = EvalCtx(ctx.sv_vals.updated(svname, svval), ctx.sv_uninit - svname, ctx.locv_vals, ctx.eparam_vals)
        })
      }
    )
    ctx
  }

  def evalEvent(event: Event, ctx: EvalCtx, clink: ContractLinking) : EvalCtx = {
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
