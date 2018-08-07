package miniL4.interpreter

import interpreter.Data
import miniL4.{CONTRACT_ROLE, Name, Real}

case class L4Event(
                    eventName: Name,
                    roleName: Name = CONTRACT_ROLE,
                    timeStamp: Real = 0,
//                    params: Seq[Data] = List(),
                    paramVals: Map[Name,Data] = Map.empty[Name,Data]
                  ) { }
