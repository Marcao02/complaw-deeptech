package miniL4.interpreter

import miniL4.{CONTRACT_ROLE, Name, Real}

case class L4Event( eventName: Name,
                    roleName: Name = CONTRACT_ROLE,
                    timeStamp: Real = 0,
                    paramVals: Map[Name,RTData] = Map.empty[Name,RTData]
                  ) { }



