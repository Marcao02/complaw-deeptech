package interpreter

import ast.{Name, TMap, TSet}

case class EvalCtx( sv_vals: TMap[Name,Data],
                    sv_uninit: TSet[Name],
                    locv_vals: TMap[Name,Data],
                    eparam_vals : TMap[Name,Data]) {

  def withLocalVarsUpdated(locv_vals2 : TMap[Name,Data]): EvalCtx = {
    EvalCtx(this.sv_vals, this.sv_uninit, this.locv_vals ++ locv_vals2, this.eparam_vals)
  }

}
