package indy

import indy.sexpr.exceptions.UnbalancedException
import org.scalatest.FunSuite
//import indy.sexpr.parse._
import indy.sexpr.parse2._
import indy.srcLocation.Loc.LinPos
import indy.sexpr.unparse._
import miniL4.ast.SExprToAST

import scala.io.Source


class test_sexpr  extends FunSuite {
  def atest(s1:String,s2:String) : Unit = {
    val (parsed, _) = parseStr(s1)
    val back_to_str = parsed.toString
    val name = s"parsing $s1: " + back_to_str + " == " + s2
    test(name) {
      assert(back_to_str == s2)
    }

    val name2 = s"unparse(parse($s1)) == $s1"
    test(name2) {
      val (parsed, locator) = parseStringTop(s1)
      val s3 = unparse(parsed, locator)
      println(s1)
      println(s3)
      assert(s3 == s1)
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

//  atest(" ; blah blah\n", "; blah blah") // fails
  atest("()", "()")
  atest("(\n)", "()")
  atest("[()()]", "[() ()]")
  atest("((()))", "((()))")
  atest("(a (bcd  e) f)", "(a (bcd e) f)")
  atest("(a bcd e  )", "(a bcd e)")
  atest( "[a bcd e]", "[a bcd e]" )
  atest("(a bcd e fgh)", "(a bcd e fgh)")
  atest("{(a b) (cd)}", "{(a b) (cd)}")
  unbalanced("(a b", 4)
  unbalanced("a b)", 3)
  unbalanced("[a b)", 4)
  atest("([a]{b}cd e)", "([a] {b} cd e)")
//  atest("a<=b<=c d", "(a <= b <= c d)")
//  atest("a.b.c.d", "(a . b . c . d)")
//  atest("a (b=c')", "(a (b = c'))")
//  atest("'this is a string literal'", s2 = "('this is a string literal')")
//  atest("`this is a string literal`", s2 = "(`this is a string literal`)")
//  atest("\"this is a string literal\"", s2 = "(\"this is a string literal\")")
//  atest("a.b .07 9m 1.2 1.a", s2 = "(a . b 0.07 9m 1.2 1.0 . a)")
}

class test_contract_files extends FunSuite {
  val toTest = List(
    "/Users/dustin/git-projects/legalese/complaw-deeptech/L4/pyL4/examples/src_sexpr/serious/SAFE_cap_discount_faithfull.l4",
    "/Users/dustin/git-projects/legalese/complaw-deeptech/L4/pyL4/examples/src_sexpr/serious/SAFE.l4",
    "/Users/dustin/git-projects/legalese/complaw-deeptech/L4/pyL4/examples/src_sexpr/from_academic_lit/hvitved_lease.l4",

    "/Users/dustin/git-projects/legalese/complaw-deeptech/L4/pyL4/examples/src_sexpr/from_academic_lit/hvitved_printer.l4",
    "/Users/dustin/git-projects/legalese/complaw-deeptech/L4/pyL4/examples/src_sexpr/from_academic_lit/prisacariu_schneider_abdelsadiq_Internet_provision_with_renew.l4"

  )
  for(filePath <- toTest) {
    test(filePath + "  parses"){
      val (expr, locator) = parseFile(filePath)
      unparse(expr, locator)
      val s2a = new SExprToAST(locator)
      s2a.contractToAST(expr)
      // NOTE that unparse does not produce EXACTLY the same string, because of lossy conversion such as
      // 3 -> 3.0
      // 3.00 => 3.0
      // (since we don't have Int types yet)

//      val bufferedSource = Source.fromFile(filePath)
//      val s = bufferedSource.mkString
//      bufferedSource.close
//      println(unparse(expr, locator))
//      assertResult(s)(unparse(expr, locator))
    }
  }
}
