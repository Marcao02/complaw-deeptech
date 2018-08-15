package miniL4.interpreter

import miniL4.Real

// "RT" for RunTime
// This is because Scala 2.X doesn't have union types, and because using Data = Any earlier bit me (my code had an
// unchecked type error).
sealed abstract class RTData
case class RTReal(it:Real) extends RTData {
  override def toString: String = it.toString

  override def equals(obj: scala.Any): Boolean = obj match {
    case x:Real => x == it
    case x:RTReal => x.it == it
  }
}
case class RTBool(it:Boolean) extends RTData {
  override def toString: String = it.toString

  override def equals(obj: scala.Any): Boolean = obj match {
    case x:Boolean => x == it
    case x:RTBool => x.it == it
  }
}
//case class RTString(it:String) extends RTData
object RTData {
  val rttrue = RTBool(true)
  val rtfalse = RTBool(false)
}