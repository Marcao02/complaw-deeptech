package miniL4.interpreter

import miniL4.interpreter.RTData.RTData
import miniL4.{CONTRACT_ROLE}
import indy.type_abbrevs._

case class L4Event( eventName: Name,
                    roleName: Name = CONTRACT_ROLE,
                    timeStamp: Real = 0,
                    paramVals: Map[Name,RTData] = Map.empty[Name,RTData]
                  ) { }



