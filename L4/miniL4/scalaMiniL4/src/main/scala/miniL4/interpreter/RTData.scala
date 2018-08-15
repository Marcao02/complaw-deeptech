package miniL4.interpreter

import miniL4.{Name, Real, TMap}


// "RT" for RunTime
// Did the commented-out version below (wrapping primitive values in an ADT) because Scala 2.X doesn't have union types,
// and because using Data = Any earlier bit me (my code had unchecked type errors).
// However, then I learned about Scala's AnyVal, and decided that's a good middle ground;
// --like RTData = RTDataWrapped, has more typechecking than Any,
// --like RTData = Any, introduces no inconvenience or overhead (which might matter for symbolic execution and large testing suites).
object RTData {

  type RTData = AnyVal
  type RTBool = Boolean
  type RTReal = Real
  // just identity
  val RTBool = (x: Boolean) => x
  // just identity
  val RTReal = (x: Real) => x
  val rttrue = true
  val rtfalse = false
  val fnInterps: TMap[Name, Seq[RTData] => RTData] = Map(
    '>= -> wrapping((x: Seq[Real]) => x(0) >= x(1)),
    '<= -> wrapping((x: Seq[Real]) => x(0) <= x(1)),
    '+ -> wrapping((x: Seq[Real]) => x(0) + x(1)),
    '- -> wrapping((x: Seq[Real]) => x(0) - x(1)),
    'not -> wrapping((x: Seq[Boolean]) => !x(0)),
    'and -> wrapping((x: Seq[Boolean]) => x(0) && x(1)),
    'or -> wrapping((x: Seq[Boolean]) => x(0) || x(1))
  )

  // just identity
  def wrapping[D, R](fn: Seq[D] => R): (Seq[RTData] => RTData) = fn.asInstanceOf[Seq[RTData] => RTData]
}

//sealed abstract class RTDataWrapped
//case class RTRealWrapped(it:Real) extends RTDataWrapped {
//  override def toString: String = it.toString
//
//  override def equals(obj: scala.Any): Boolean = obj match {
//    case x:Real => x == it
//    case x:RTRealWrapped => x.it == it
//  }
//}
//case class RTBoolWrapped(it:Boolean) extends RTDataWrapped {
//  override def toString: String = it.toString
//
//  override def equals(obj: scala.Any): Boolean = obj match {
//    case x:Boolean => x == it
//    case x:RTBoolWrapped => x.it == it
//  }
//}
// object {
//  type RTData = RTDataWrapped
//  val rttrue = RTBoolWrapped(true)
//  val rtfalse = RTBoolWrapped(false)
//  val fnInterps : TMap[Name, Seq[RTData] => RTData]= Map(
//    '>= -> wrapping((x:Seq[Real]) => x(0) >= x(1)),
//    '<= -> wrapping((x:Seq[Real]) => x(0) <= x(1)),
//    '+ -> wrapping((x:Seq[Real]) => x(0) + x(1)),
//    '- -> wrapping((x:Seq[Real]) => x(0) - x(1)),
//    'not -> wrapping((x:Seq[Boolean]) => !x(0)),
//    'and -> wrapping((x:Seq[Boolean]) => x(0) && x(1)),
//    'or -> wrapping((x:Seq[Boolean]) => x(0) || x(1))
//  )
//  def wrapping[D,R](fn:Seq[D] => R) (args:Seq[RTDataWrapped]) : RTDataWrapped = {
//    val unwrapped : Seq[D] = args.map {
//      case RTBoolWrapped(it) => it.asInstanceOf[D]
//      case RTRealWrapped(it) => it.asInstanceOf[D]
//      //      case RTString(it) => it.asInstanceOf[D]
//    }
//    val fnval = fn(unwrapped)
//    fnval match {
//      case rval:Real => RTRealWrapped(rval)
//      case bval:Boolean => RTBoolWrapped(bval)
//    }
//  }