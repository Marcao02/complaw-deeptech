package miniL4.interpreter

import miniL4.{Name, TMap, TSet}
import miniL4.interpreter.RTData.RTData
import miniL4.ast.SituationDef

case class EvalCtx(sv_vals: TMap[Name,RTData],
                   sv_uninit: TSet[Name],
                   locv_vals: TMap[Name,RTData],
                   eparam_vals : TMap[Name,RTData],
                   cur_sit : SituationDef ) {
  def withStateVarValsUpdated(sv_vals2 : TMap[Name,RTData]): EvalCtx = {
    EvalCtx(this.sv_vals ++ sv_vals2, this.sv_uninit.diff(sv_vals2.keys.toSet), this.locv_vals, this.eparam_vals, this.cur_sit)
  }

  def withLocalVarsUpdated(locv_vals2 : TMap[Name,RTData]): EvalCtx = {
    EvalCtx(this.sv_vals, this.sv_uninit, this.locv_vals ++ locv_vals2, this.eparam_vals, this.cur_sit)
  }

  def withEventParamsUpdated(eparam_vals2 : TMap[Name,RTData]): EvalCtx = {
    EvalCtx(this.sv_vals, this.sv_uninit, this.locv_vals, eparam_vals2, this.cur_sit)
  }

  def withCurSitUpdated(sitDef:SituationDef): EvalCtx = {
    EvalCtx(this.sv_vals, this.sv_uninit, this.locv_vals, this.eparam_vals, sitDef)
  }

}
