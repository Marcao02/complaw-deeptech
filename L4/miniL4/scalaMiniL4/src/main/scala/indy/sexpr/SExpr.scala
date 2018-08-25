package indy.sexpr

import indy.sexpr.BracketType.BracketType
import indy.srcLocation.{HeavySrcSpan, LCSpan, Loc, Locator}
import indy.srcLocation.Loc._

abstract sealed class SExpr(linspan:LinSpan) {
  def lcSpan(l:Locator) : LCSpan = LCSpan(l.linPosToLCPos(linspan._1), l.linPosToLCPos(linspan._2))
  def located(l:Locator) : LocatedSExpr = LocatedSExpr(this, HeavySrcSpan(l.filePath, lcSpan(l)))
}

// just to pass to exceptions, not to create in bulk
case class LocatedSExpr(expr:SExpr, loc:HeavySrcSpan) {
  def filePath = loc.filepath
}

case class BrackExpr(ch:Seq[SExpr], brackType: BracketType, linspan:LinSpan) extends SExpr(linspan) {
  override def toString: String = {
    if(brackType == BracketType.fileToplevel)
      ch.mkString("", " ", "")
    else {
      val lg = bracketTypeToLeftGrouper(brackType)
      ch.mkString(lg.toString, " ", grouper_map(lg).toString)
    }
  }
}


abstract sealed class LeafExpr(v:Any, linspan:LinSpan) extends SExpr(linspan) {
  override def toString: String = v.toString
}
case class Token(v:String, linspan:LinSpan) extends LeafExpr(v, linspan:LinSpan)
case class StrLit(v:String, quoteChar:Char, linspan:LinSpan) extends LeafExpr(v, linspan:LinSpan) {
  override def toString: String = s"$quoteChar$v$quoteChar"
}
case class Comment(v:String, linspan:LinSpan) extends LeafExpr(v, linspan:LinSpan) {
  override def toString: String = v.toString // comment start char included already
  //  override def toString: String = s";$v"
}
// giving number literals their own case, as opposed to having them be Tokens, allows us to use '.' as a special character.
// note: converting ints to Double and using Double's toString messes up column numbers slightly
case class NumberLit(v:Double, linspan:LinSpan) extends LeafExpr(v, linspan:LinSpan)