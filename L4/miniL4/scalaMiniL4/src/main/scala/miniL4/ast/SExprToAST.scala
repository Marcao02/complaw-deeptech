package miniL4.ast
import indy.sexpr._
import indy.srcLocation.Locator
import miniL4.SExprParseError

import scala.collection.mutable

class SExprToAST(val locator: Locator) {
  def l(e:SExpr) : LocatedSExpr = e.located(locator)

  def dispatch(e:SExpr) : ASTNode = {
    e match {
      case e@Comment(s,span) => {
        ()
      }
      case e@Token(s,span) => {
        ()
      }
      case e@StrLit(s,_,span) => {
        ()
      }
      case e@NumberLit(num, _) => {
        ()
      }
      case e@BrackExpr(_,_,_) => {
        e.headToken(locator) match {
          case t@Token("StateVars", span) => {
            println("found StateVars")
          }
          case t@Token("Dynamics", span) => {
            println("found Dynamics")
          }
          case _ => {
            ()
          }
        }
      }
    }

    BullshitASTNode

  }

  def contractToAST(e:SExpr) : Unit = {
//    val builder = Seq.newBuilder[ToplevelNode]
    val builder = Seq.newBuilder[ASTNode]
    e match {
      case e:BrackExpr => {
        for(toplevel_expr <- e.ch)
          builder += dispatch(toplevel_expr)
      }
      case _ => throw new SExprParseError(l(e), "Expected a bracket expression.")
    }
//    Contract(builder.result())
  }

//  def toplevelToAST(e: BrackExpr) : ToplevelNode = {
//    e.ch.head match {
//      case Token("StateVars", span) => {
//        println("found StateVars")
//      }
//      case _ => {}
//    }
//  }
}
