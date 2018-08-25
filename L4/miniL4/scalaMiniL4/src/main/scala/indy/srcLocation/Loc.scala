package indy.srcLocation

object Loc {
  type LinPos = Int
  type LinSpan = (Int, Int)
}

case class LCPos(line:Int,coln:Int) {
  override def toString: String = s"(line ${line}, coln ${coln})"
  def plusColn(diff:Int) : LCPos = LCPos(line, coln + diff)
}
case class LCSpan(left:LCPos, right:LCPos) {
  override def toString: String = s"[${left} to ${right}]"
}

sealed abstract class Loc
  case object NoLoc extends Loc
  sealed abstract class WithPath(filepath:String) extends Loc
    case class HeavySrcSpan(filepath:String, span:LCSpan) extends WithPath(filepath) {
      override def toString: String = s"$span in $filepath"
    }
    case class HeavySrcPos(filepath:String, pos:LCPos, generated:Boolean = false) extends WithPath(filepath) {
      override def toString: String = s"$pos in $filepath"
    }

trait Locatable {
  val loc:Loc
}
