package miniL4.interpreter

import interpreter.Data
import miniL4.{Name, Real, CONTRACT_ROLE}

case class L4Event(
                    eventName: Name,
                    roleName: Name = CONTRACT_ROLE,
                    timeStamp: Real = 0,
                    params: Seq[Data] = List()
                  ) { }
