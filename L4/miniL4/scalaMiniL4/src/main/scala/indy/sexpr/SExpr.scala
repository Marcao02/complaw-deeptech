package indy.sexpr

import indy.sexpr.BracketType.BracketType

abstract sealed class SExpr(pos:LinArea) {
  def lcArea(l:Locator) : LCArea = (l.linPosToLCPos(pos._1), l.linPosToLCPos(pos._2))
}

case class BrackExpr(ch:Seq[SExpr], brackType: BracketType, pos:LinArea) extends SExpr(pos) {
  override def toString: String = {
    val lg = bracketTypeToLeftGrouper(brackType)
    ch.mkString(lg.toString, " ", grouper_map(lg).toString)
  }
}
abstract sealed class LeafExpr(v:Any, pos:LinArea) extends SExpr(pos) {
  override def toString: String = v.toString
}
case class Token(v:String, pos:LinArea) extends LeafExpr(v, pos:LinArea)
case class StrLit(v:String, quoteChar:Char, pos:LinArea) extends LeafExpr(v, pos:LinArea) {
  override def toString: String = s"$quoteChar$v$quoteChar"
}
case class Comment(v:String, pos:LinArea) extends LeafExpr(v, pos:LinArea) {
  override def toString: String = s";$v"
}
// giving number literals their own case, as opposed to having them be Tokens, allows us to use '.' as a special character.
case class NumberLit(v:Double, pos:LinArea) extends LeafExpr(v, pos:LinArea)