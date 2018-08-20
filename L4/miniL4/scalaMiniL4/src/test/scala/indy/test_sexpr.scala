package indy

import indy.sexpr.LinPos
import indy.sexpr.exceptions.UnbalancedException
import org.scalatest.FunSuite
import indy.sexpr.parse._


class test_sexpr  extends FunSuite {
  def atest(s1:String,s2:String) : Unit = {
    val (parsed, _) = parseStr(s1)
    val back_to_str = parsed.toString
    val name = s"parsing $s1: " + back_to_str + " == " + s2
    test(name) {
      assert(back_to_str == s2)
    }
  }
  def unbalanced(s:String, pos:LinPos) : Unit = {
    val name = s + " failure"
    test(name) {
      try {
        parseStr(s)
      }
      catch {
        case e:UnbalancedException => assert(e.linpos == pos, s"Correctly threw UnbalancedException but expected file position ${pos} is different from announced ${e.linpos}." )
        case e:Throwable => fail(s"Unpected exception ${e}.")
      }
      succeed
    }
  }

  atest("a bcd e  ", "(a bcd e)")
  atest( "[a bcd e]", "([a bcd e])" )
  atest("(a bcd e fgh)", "((a bcd e fgh))")
  atest("(a b) (cd)", "((a b) (cd))")
  unbalanced("(a b", 4)
  unbalanced("a b)", 3)
  unbalanced("[a b)", 4)
  atest("[a]{b}cd e", "([a] {b} cd e)")
  atest("a<=b<=c d", "(a <= b <= c d)")
  atest("a.b.c.d", "(a . b . c . d)")
  atest("a (b=c')", "(a (b = c'))")
  atest("'this is a string literal'", s2 = "('this is a string literal')")
  atest("`this is a string literal`", s2 = "(`this is a string literal`)")
  atest("\"this is a string literal\"", s2 = "(\"this is a string literal\")")
  atest("a.b .07 9m 1.2 1.a", s2 = "(a . b 0.07 9m 1.2 1.0 . a)")
}

class test_contract_files extends FunSuite {
  val toTest = List(
    "/Users/dustin/git-projects/legalese/complaw-deeptech/L4/pyL4/examples/src_sexpr/serious/SAFE_cap_discount_faithfull.l4",
    "/Users/dustin/git-projects/legalese/complaw-deeptech/L4/pyL4/examples/src_sexpr/from_academic_lit/hvitved_lease.l4",
    "/Users/dustin/git-projects/legalese/complaw-deeptech/L4/pyL4/examples/src_sexpr/from_academic_lit/hvitved_printer.l4"
  )
  for(filepath <- toTest) {
    test(filepath + "  parses"){
      parseFile(filepath)
    }
  }
}
