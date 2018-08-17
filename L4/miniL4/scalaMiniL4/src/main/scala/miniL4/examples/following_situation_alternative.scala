package miniL4.examples

import miniL4.ast.astutil._
import miniL4.ast._
import miniL4.interpreter.L4Event
import miniL4.typechecker.stdlibTyping.stdDataTypes.{realDType,posRealDType}

import miniL4.examples.abbrevs._

object following_situation_alternative extends TestExample {
  private val me = 'Me
  private val justme = List(me)

  val contract : Contract = Contract(List(
    sd('Start, List(
      eer('Enter, justme, ntc, None, List(hp2rp('enter_param)))
    )),

    sd('AfterEnter, List(
      eer('Enter, justme, ntc, None, List(hp2rp('enter_param)),
           Some( geq(hp2rp('enter_param), 'enter_param)))
    ) ),

    ehd('Enter, 'AfterEnter, List(), List(('enter_param, realDType)))
  ))

  val traces = List(
    List(
      L4Event('Enter, me, 0, Map('enter_param -> d(9))),
      L4Event('Enter, me, 0, Map('enter_param -> d(9)))
    )
  )

  val exceptionTraces = List(
    List(
      L4Event('Enter, me, 0, Map('enter_param -> d(9))),
      L4Event('Enter, me, 0, Map('enter_param -> d(8)))
    )
  )

}