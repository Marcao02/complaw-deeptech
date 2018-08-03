//    val locvals, eparamvals  = Map.empty[Name,Data]
//    var ctx = EvalCtx(sv_vals, sv_uninitialized, locvals, eparamvals)
//    val (sv_vals, sv_uninitialized, sv_untouched) = (Map.empty[Name,Data], Set.empty[Name], Map.empty[Name,Term])
// first divide the StateVar into a map of will-be-initialized vars and a set of uninitialized vars.
//    val (sv_vals, sv_uninitialized) = {
//      val (sv_init, sv_uninit) = clink.stateVarDefs.partition({ case (name, svd) => svd.initVal.nonEmpty })
//      (sv_init.mapValues( (svd:StateVarDef) => evalTerm(svd.initVal.get, ctx, clink)), sv_uninit.keySet)
//    }
//    val (sv_vals, sv_uninitialized) = {
//      val (sv_init, sv_uninit) = clink.stateVarDefs.partition({ case (name, svd) => svd.initVal.nonEmpty })
//      (sv_init.mapValues( (svd:StateVarDef) => evalTerm(svd.initVal.get, ctx, clink)), sv_uninit.keySet)
//    }