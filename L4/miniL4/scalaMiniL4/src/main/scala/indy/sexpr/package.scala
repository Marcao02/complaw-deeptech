package indy

import indy.sexpr.BracketType
import indy.sexpr.BracketType.BracketType


package object sexpr {

  object BracketType extends Enumeration {
    type BracketType = Value
    val roundBrack : BracketType = Value
    val squareBrack : BracketType = Value
    val curlyBrack : BracketType = Value
    val fileToplevel : BracketType = Value
    val implicitBrack : BracketType = Value
  }

  val LINE_COMMENT_START_CHAR = ';'

  val COMMENT_LITERAL_TOKEN = 'COMMENT // the head of a comment SExpr (the result of a LINE_COMMENT_START_CHAR comment)  
  val PASTE_DIRECTIVE_TOKEN = 'PASTE
  val STRING_LITERAL_TOKEN = 'STRLIT

  val left_groupers = Set('(','{','[','‹','❪')
  val right_groupers = Set(')','}',']','›','❫')
  val grouper_map : Map[Char,Char] = Map(
    '(' -> ')',
    '{' -> '}',
    '[' -> ']',
    '‹' -> '›',
    '❪' -> '❫',
    '"' -> '"',
    '`' -> '`',
    LINE_COMMENT_START_CHAR -> '\n')
  val quotelike : Set[Char] = Set("'".toCharArray()(0),'"','`')

  val double_char_seqs_that_split_words_only = Set(":=","+=","-=","*=","==","<=", ">=", "->", "=>", "<-")
  val single_chars_that_split_words = Set(':','=',',','.')

  val right_grouper_to_bracket_type = Map(
    ')' -> BracketType.roundBrack,
    ']' -> BracketType.squareBrack,
    '}' -> BracketType.curlyBrack
  )

  val bracketTypeToLeftGrouper = Map(
    BracketType.roundBrack -> '(',
    BracketType.squareBrack -> '[',
    BracketType.curlyBrack -> '{'
//    BracketType.fileToplevel -> '('
  )
  //  val all_head_tokens = quotelike ++ left_groupers ++ Set(LINE_COMMENT_START_CHAR)

}
