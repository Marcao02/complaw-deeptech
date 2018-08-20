package indy.sexpr

import exceptions.{UnbalancedException, UnbalancedExceptionLC}

import scala.io.Source
import util.control.Breaks._


/*
 a bit fewer parens:
 if the last non-whitespace part of a line is ...
 and the first non-whitespace part of the next line is not a grouping symbol, a quote, or a line comment character,
 then we wrap the whole thing (minus any line comment at end) in implicit parens
*/

object parse {
  def parseStr(s:String, debug:Boolean=false,
               stripComments:Boolean=false,
               allowPrimedNames:Boolean=true,
               lineBreakChar:String = "\n"
              ) : (SExpr, IndexedSeq[Int]) = {

    var lineStartIndices = IndexedSeq(0)

    /*
    Return the sequence of SExpr's S and index i such that parsing starting from position i generates the
    SExpr's S until a close bracket at index i with no matching open bracket is encountered.
     */
    def parse(i_start:Int, _line:Int, _coln:Int, expecting:Option[Char]) : (Seq[SExpr],Int,Int,Int) = {
      var builder = Seq.newBuilder[SExpr]
      var i = i_start
      var token = ""
      var start_token_ind : Option[LinPos] = None
      var quoteExpecting : Option[Char] = None
      var (line,coln) = (_line,_coln)
      var lineCommentStart : Option[Int] = None
      var a : Char = '?' // value arbitrary. will be written before read.

      def appendToken(): Unit = {
        assert(token.nonEmpty)
        builder += {
          try {
            val num = token.toDouble
            NumberLit(num, (start_token_ind.get, start_token_ind.get + token.length))
          } catch {
            case _:NumberFormatException => Token(token, (start_token_ind.get, start_token_ind.get + token.length))
          }
        }
        token = ""
        start_token_ind = None
      }
      def maybeAppendToken(): Unit = if (token.nonEmpty)  appendToken()

      def advance(): Unit = {
        if(a.toString == lineBreakChar) {
          line += 1
          coln = 0
          lineStartIndices = lineStartIndices :+ (i+1)
        }
        else
          coln += 1
        i += 1
      }
      def take(): Unit = {
        assert(lineCommentStart.nonEmpty || quoteExpecting.nonEmpty || !a.isWhitespace)
        token += a
        advance()
      }

      def notEnd(): Boolean = i < s.length - 1

      while (i < s.length) {
        breakable {
          a = s(i)

          if (lineCommentStart.nonEmpty) {
            if (a.toString == lineBreakChar) {
              if(!stripComments) {
                builder += Comment(token, (i_start, i))
              }
              token = ""
              advance()
              lineCommentStart = None
            }
            else take()
            break
          }

          /* We are not already in a comment */
          if ( a == LINE_COMMENT_START_CHAR ) {
            lineCommentStart = Some(i)
            advance()
          }
          /* We are not in a comment */
          else if (allowPrimedNames && a.toString == "'" && quoteExpecting.isEmpty && i > 0 && s(i - 1).isLetterOrDigit) {
            take()
            appendToken()
          }
          else if (quotelike.contains(a)) {
            if (quoteExpecting.nonEmpty) {
              if (quoteExpecting.contains(a)) {
                builder += StrLit(token, a, (start_token_ind.get, i - 1))
                token = ""
                start_token_ind = None
                quoteExpecting = None
                advance()
              }
              else take()
            }
            else {
              assert(token.isEmpty, s"Whitespace must precede a string literal, but found ${token}. See ${LCPos(line,coln)}.")
              quoteExpecting = Some(a)
              start_token_ind = Some(i + 1)
              advance()
            }
          }
          else if (quoteExpecting.nonEmpty) take()
          /* We are not in a comment or string literal */
          else {
            if (left_groupers.contains(a)) {
              maybeAppendToken()
              val (ch, next_linpos, next_line, next_coln) = parse(i + 1, line, coln, Some(grouper_map(a)))
              if (s(next_linpos) != grouper_map(a)) {
                throw UnbalancedExceptionLC(Some(grouper_map(a)), Some(s(next_linpos)), LCPos(next_line, next_coln))
              }
              builder += BrackExpr(ch, right_grouper_to_bracket_type(s(next_linpos)), (i, next_linpos))
              i = next_linpos + 1
              coln = next_coln + 1
              line = next_line
            }
            else if (right_groupers.contains(a)) {
              if (!expecting.contains(a))
                throw UnbalancedException(expecting, Some(a), i)
              maybeAppendToken()
              return (builder.result(), i, line, coln)
            }
            else if (notEnd() && double_char_seqs_that_split_words_only.contains(s"${a.toString}${s(i + 1).toString}")) {
              maybeAppendToken()
              start_token_ind = Some(i)
              token += a.toString + s(i + 1).toString
              appendToken()
              i += 2
              coln += 2
            }
            else if (single_chars_that_split_words.contains(a)) {
              // if it's a '.', check if we're in the middle or possible beginning of a decimal number.
              if( a == '.' && token.forall(_.isDigit) && notEnd() && s(i+1).isDigit ) {
                if (token.isEmpty)
                  start_token_ind = Some(i)
                take()
              }
              else {
                maybeAppendToken()
                token = a.toString
                start_token_ind = Some(i)
                appendToken()
                advance()
              }
            }
            else if (a.isWhitespace) {
              maybeAppendToken()
              advance()
            }
            else {
              if (token.isEmpty)
                start_token_ind = Some(i)
              take()
            }
          }
        }
      }

      if(expecting.nonEmpty)  throw UnbalancedException(None, expecting, i)
      maybeAppendToken()
      (builder.result(), s.length - 1, line, coln)
    }

    val (sexpr, ind, line, coln) = parse(0, 0, 0, None)
    if(line > 0) println(s"Parsed ${line + 1} lines.")
    assert(ind == s.length - 1, s"Parsed ${ind+1} characters while expecteing ${s.length}")
    (BrackExpr(sexpr, BracketType.fileToplevel, (0,s.length - 1)), lineStartIndices)
  }

  def parseFile(filePath:String, debug:Boolean=false, stripComments:Boolean=false) : (SExpr, Locator) = {
    val bufferedSource = Source.fromFile(filePath)
    val s = bufferedSource.mkString
    bufferedSource.close
    val (parsed, lineStartIndices) = parseStr(s, debug, stripComments)
    (parsed, new Locator(filePath, lineStartIndices))

  }
}
