import miniL4._
import miniL4.interpreter.{RTBool, RTData, RTReal}


package object interpreter {
//  val fnInterps : TMap[Name, Seq[Data] => Data]= Map(
//    '>= -> ((x:Seq[Data]) => x(0).asInstanceOf[Real] >= x(1).asInstanceOf[Real]),
//    '<= -> ((x:Seq[Data]) => x(0).asInstanceOf[Real] <= x(1).asInstanceOf[Real]),
//    '+ -> ((x:Seq[Data]) => x(0).asInstanceOf[Real] + x(1).asInstanceOf[Real]),
//    '- -> ((x:Seq[Data]) => x(0).asInstanceOf[Real] - x(1).asInstanceOf[Real]),
//    'not -> ((x:Seq[Data]) => !x(0).asInstanceOf[Boolean]),
//    'and -> ((x:Seq[Data]) => x(0).asInstanceOf[Boolean] && x(1).asInstanceOf[Boolean]),
//    'or -> ((x:Seq[Data]) => x(0).asInstanceOf[Boolean] && x(1).asInstanceOf[Boolean])
//  )

  val fnInterps : TMap[Name, Seq[RTData] => RTData]= Map(
    '>= -> wrapping((x:Seq[Real]) => x(0) >= x(1)),
    '<= -> wrapping((x:Seq[Real]) => x(0) <= x(1)),
    '+ -> wrapping((x:Seq[Real]) => x(0) + x(1)),
    '- -> wrapping((x:Seq[Real]) => x(0) - x(1)),
    'not -> wrapping((x:Seq[Boolean]) => !x(0)),
    'and -> wrapping((x:Seq[Boolean]) => x(0) && x(1)),
    'or -> wrapping((x:Seq[Boolean]) => x(0) || x(1))
  )

  def wrapping[D,R](fn:Seq[D] => R) (args:Seq[RTData]) : RTData = {
    val unwrapped : Seq[D] = args.map( x => x match {
      case RTBool(it) => it.asInstanceOf[D]
      case RTReal(it) => it.asInstanceOf[D]
//      case RTString(it) => it.asInstanceOf[D]
    })
    val fnval = fn(unwrapped)
    fnval match {
      case rval:Real => RTReal(rval)
      case bval:Boolean => RTBool(bval)
    }
  }

//  val datatypePreds : TMap[Datatype, Data => Boolean]= Map(
//  )
}
