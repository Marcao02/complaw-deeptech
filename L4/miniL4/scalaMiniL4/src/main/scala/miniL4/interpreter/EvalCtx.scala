package miniL4.interpreter

import interpreter.Data
import miniL4._
import miniL4.ast.{SituationDef}

case class EvalCtx( sv_vals: TMap[Name,Data],
                    sv_uninit: TSet[Name],
                    locv_vals: TMap[Name,Data],
                    eparam_vals : TMap[Name,Data],
                    cur_sit : SituationDef ) {
  def withLocalVarsUpdated(locv_vals2 : TMap[Name,Data]): EvalCtx = {
    EvalCtx(this.sv_vals, this.sv_uninit, this.locv_vals ++ locv_vals2, this.eparam_vals, this.cur_sit)
  }

  def withEventParamsUpdated(eparam_vals2 : TMap[Name,Data]): EvalCtx = {
    EvalCtx(this.sv_vals, this.sv_uninit, this.locv_vals, eparam_vals2, this.cur_sit)
  }

}
