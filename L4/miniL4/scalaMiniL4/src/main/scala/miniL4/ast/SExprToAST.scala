package miniL4.ast
import indy.sexpr._
import indy.srcLocation.Locator
import miniL4.ParseError

import scala.collection.mutable

class SExprToAST(val locator: Locator) {
//  def l(e:SExpr) : LocatedSExpr = e.located(locator)

//  def toAST(e:SExpr) : Contract = {
//    val builder = Seq.newBuilder[ToplevelNode] // new mutable.Builder[ToplevelNode, Seq[ToplevelNode]] =
//    e match {
//      case e:BrackExpr => {
//        e.ch.foreach(toplevel => {
//          toplevel
//        })
//      }
//      case _ => throw new ParseError(l(e), "Expected a bracket expression.")
//    }
//    Contract(builder.result())
//  }
}
