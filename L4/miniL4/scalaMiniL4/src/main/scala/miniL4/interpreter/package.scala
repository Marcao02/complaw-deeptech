import miniL4._


package object interpreter {
  type Data = Any

  val fnInterps : TMap[Name, Seq[Data] => Data]= Map(
    '>= -> ((x:Seq[Data]) => x(0).asInstanceOf[Real] >= x(1).asInstanceOf[Real]),
    '<= -> ((x:Seq[Data]) => x(0).asInstanceOf[Real] <= x(1).asInstanceOf[Real]),
    '+ -> ((x:Seq[Data]) => x(0).asInstanceOf[Real] + x(1).asInstanceOf[Real]),
    '- -> ((x:Seq[Data]) => x(0).asInstanceOf[Real] - x(1).asInstanceOf[Real]),
    'not -> ((x:Seq[Data]) => !x(0).asInstanceOf[Boolean]),
    'and -> ((x:Seq[Data]) => x(0).asInstanceOf[Boolean] && x(1).asInstanceOf[Boolean]),
    'or -> ((x:Seq[Data]) => x(0).asInstanceOf[Boolean] && x(1).asInstanceOf[Boolean])
  )

//  val datatypePreds : TMap[Datatype, Data => Boolean]= Map(
//  )
}