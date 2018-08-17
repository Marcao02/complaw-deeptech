package miniL4.typechecker.stdlibTyping

import indy.type_abbrevs._
import miniL4.ast.Datatype
import miniL4.typechecker.{FnType, OverloadedFnType, SimpleFnType}
import miniL4.typechecker.stdlibTyping.stdDataTypes._

object stdFnTypes {
//  val names2type : TSet[(List[Name],FnType)] = Set(
//    (List('+,'*), OverloadedFnType(Set(
//      SimpleFnType(List(realDType,realDType,realDType)),
//      SimpleFnType(List(posRealDType,posRealDType,posRealDType)),
//      SimpleFnType(List(nonnegRealDType,nonnegRealDType,nonnegRealDType))
//    ))),
//    (List('>=,'<=), OverloadedFnType(Set(
//      SimpleFnType(List(realDType,realDType,boolDType))
//    )))
//  )

  val name2type : TSet[(Name,FnType)] = Set(
    ('-, OverloadedFnType(Set(
     SimpleFnType(List(realDType,realDType,realDType))
    ))),
    ('+, OverloadedFnType(Set(
      SimpleFnType(List(realDType,realDType,realDType)),
      SimpleFnType(List(posRealDType,posRealDType,posRealDType)),
      SimpleFnType(List(nonnegRealDType,nonnegRealDType,nonnegRealDType))
    ))),
    ('*, OverloadedFnType(Set(
      SimpleFnType(List(realDType,realDType,realDType)),
      SimpleFnType(List(posRealDType,posRealDType,posRealDType)),
      SimpleFnType(List(nonnegRealDType,nonnegRealDType,nonnegRealDType))
    ))),
    ('>=, OverloadedFnType(Set(
      SimpleFnType(List(realDType,realDType,boolDType))
    ))),
    ('<=, OverloadedFnType(Set(
      SimpleFnType(List(realDType,realDType,boolDType))
    ))),
    ('and, OverloadedFnType(Set(
      SimpleFnType(List(boolDType,boolDType,boolDType))
    ))),
    ('or, OverloadedFnType(Set(
      SimpleFnType(List(boolDType,boolDType,boolDType))
    ))),
    ('not, OverloadedFnType(Set(
      SimpleFnType(List(boolDType,boolDType))
    )))
  )
}
