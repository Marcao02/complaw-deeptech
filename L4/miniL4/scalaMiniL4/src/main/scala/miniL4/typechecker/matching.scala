package miniL4.typechecker
import miniL4.{Name, TMap, TSet}
import miniL4.ast.{Datatype, DatatypeOpApp, DependentDatatypeOpApp}
import miniL4.interpreter.RTData
//import miniL4.typechecker.DatatypePatSubst
import miniL4.interpreter.evalL4.evalTermSimple
import miniL4.interpreter.RTData._

import util.control.Breaks._

object matching {

  def matchData(data:RTData, pat:ConstantMatchVar, subst:DatatypePatSubst = DatatypePatSubst.empty) : Option[DatatypePatSubst] = {
    if (subst.forData.contains(pat.name)) {
      if (data != subst.forData(pat.name)) None
      else Some(subst)
    }
    else
      Some(DatatypePatSubst(subst.forDatatypes, subst.forData.updated(pat.name, data)))
  }

  /* This is an inefficient version, which will do for now. */
  def matchDT(dtype:Datatype, pat:DatatypePattern, subst:DatatypePatSubst = DatatypePatSubst.empty) : Option[DatatypePatSubst] = {
    pat match {
      case FixedDatatype(dtype2) => if(dtype == dtype2) Some(subst) else None

      case mvar@DatatypeMatchVar(mvarname) => {
        if (subst.forDatatypes.contains(mvarname)) {
          if (dtype != subst.forDatatypes(mvarname)) None
          else Some(subst)
        }
        else
          Some(DatatypePatSubst(subst.forDatatypes.updated(mvarname, dtype), subst.forData))
      }

      case DatatypeOpAppPattern(op,argsPat) => {
        dtype match {
          case DatatypeOpApp(op2,args,_) => {
            if(op != op2 || args.length != argsPat.length) None
            else {
              var newsubstO : Option[DatatypePatSubst] = Some(subst)
              breakable {
                for ((child_dtype, child_pat) <- args.view.zip(argsPat) ) {
                  newsubstO = matchDT(child_dtype, child_pat, newsubstO.get)
                  if(newsubstO == None)
                    break
                }
              }
              newsubstO
            }
          }
          case _ => None
        }
      }

      case DependentDatatypeOpAppPattern(op, dataargsPat, argsPat) => {
        dtype match {
          case DependentDatatypeOpApp(op2, dataargs, args, _) => {
            if (op != op2 || args.length != argsPat.length || dataargs.length != dataargsPat.length) None
            else {
              var newsubstO: Option[DatatypePatSubst] = Some(subst)
              breakable {
                for ((child_dtype, child_pat) <- args.view.zip(argsPat)) {
                  newsubstO = matchDT(child_dtype, child_pat, newsubstO.get)
                  if (newsubstO == None)
                    break
                }
                // not sure if the break in the previous loops breaks out of just the loop, or also the surrounding
                // `breakable`, so:
                if (newsubstO == None)
                  break
                for ((child_data, child_datapat) <- dataargs.view.zip(dataargsPat)) {
                  newsubstO = matchData(child_data, child_datapat, newsubstO.get)
                  if (newsubstO == None)
                    break
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
                      subst:DatatypePatSubst = DatatypePatSubst.empty) : TSet[(Datatype, DatatypePatSubst)] = {
    allowed_datatypes.flatMap( (dt) => matchDT(dt,pat,subst) match {
      case None => None
      case Some(subst2) => Some((dt,subst2))
    })
  }

  def subtypePairMatches(pat:SubtypePairPattern, allowed_datatypes:TSet[Datatype]) : TSet[(Datatype, Datatype)] = {
    allowed_datatypes.flatMap( dt => {
      datatypeMatches(pat.left, allowed_datatypes).flatMap({ case (dtleft, subst) => {
        datatypeMatches(pat.right, allowed_datatypes, subst).flatMap({ case (dtright, subst2) => {
          pat match {
            case SimpleSubtypePairPattern(_,_) => Some((dtleft,dtright))
            case DependentSubtypePairPattern(_,_,sidecondition) =>
              if( evalTermSimple(sidecondition, subst2.forData) == rttrue ) Some((dtleft, dtright)) else None
            case HigherOrderSubtypePairPattern(_,_,sidecondition) =>
              if( sidecondition(subst2) ) Some((dtleft, dtright)) else None
          }
        }})
      }})
    })
  }



}