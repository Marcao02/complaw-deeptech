package miniL4.typechecker
import miniL4.{Name, TMap, TSet}
import miniL4.ast.{Datatype, DatatypeOpApp}
import miniL4.typechecker.DatatypePattern.DatatypePatSubst

import util.control.Breaks._

object matching {

  /* This is an inefficient version, which will do for now. */
  def matchDT(dtype:Datatype, pat:DatatypePattern, subst:DatatypePatSubst = Map.empty[Name,Any]) : Option[DatatypePatSubst] = {
    pat match {
      case FixedDatatype(dtype2) => if(dtype == dtype2) Some(subst) else None

      case mvar@ConstantMatchVar(mvarname, mvar_datatype) => {
        // TODO: check mvar_datatype
        if (subst.contains(mvarname))
          if (dtype != subst(mvarname)) None else Some(subst)
        else
          Some(subst.updated(mvarname, dtype))
      }

      case mvar@DatatypeMatchVar(mvarname) => {
        if (subst.contains(mvarname))
          if (dtype != subst(mvarname)) None else Some(subst)
        else
          Some(subst.updated(mvarname, dtype))
      }

      case DatatypeOpAppPattern(op,args) => {
        dtype match {
          case DatatypeOpApp(op2,args2,_) => {
            if(op != op2 || args2.length != args.length) None
            else {
              var newsubstO : Option[DatatypePatSubst] = Some(subst)
              breakable {
                for ((child_dtype, child_pat) <- args2.view.zip(args) ) {
                  if(newsubstO == None)
                    break
                  else
                    newsubstO = matchDT(child_dtype, child_pat, newsubstO.get)
                }
              }
              newsubstO
            }
          }
          case _ => None
        }
      }
    }

  }

  def datatypeMatches(pat:DatatypePattern, allowed_datatypes:TSet[Datatype],
                      subst:DatatypePatSubst = Map.empty[Name,Any]) : TSet[(Datatype, DatatypePatSubst)] = {
    allowed_datatypes.flatMap( (dt) => matchDT(dt,pat,subst) match {
      case None => None
      case Some(subst2) => Some((dt,subst2))
    })
  }

  def subtypePairMatches(pat:SubtypePairPattern, allowed_datatypes:TSet[Datatype]) : TSet[(Datatype, Datatype)] = {
    allowed_datatypes.flatMap( dt => {
      datatypeMatches(pat.left, allowed_datatypes).flatMap({ case (dtleft, subst) => {
        datatypeMatches(pat.right, allowed_datatypes, subst).flatMap({ case (dtright, subst2) => {
          if( pat.sidecondition(subst2) ) Some((dtleft, dtright))
          else None
        }})
      }})
    })
  }



}