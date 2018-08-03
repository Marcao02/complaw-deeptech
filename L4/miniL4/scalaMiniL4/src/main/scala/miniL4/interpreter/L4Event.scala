package miniL4.interpreter

import interpreter.Data
import miniL4.{Name, Real}

case class L4Event(
                    eventName: Name,
                    roleName: Name,
                    timeStamp: Real,
                    params: Seq[Data]
                  ) { }
