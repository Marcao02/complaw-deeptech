package indy.sexpr

import indy.srcLocation.{LCPos, Locator}

object unparse {

  def unparse(e:SExpr, l:Locator) : String = {
    var rv = ""
    def _unparse(x:SExpr, startPos: LCPos) : LCPos = {
      val span = x.lcSpan(l)
      val nextStartPos = span.left
      if(nextStartPos.line == startPos.line) {
        assert(nextStartPos.coln >= startPos.coln, s"Failed ${nextStartPos.coln} >= ${startPos.coln} at $x, line ${nextStartPos.line}")
        rv += " "*(nextStartPos.coln - startPos.coln)
      }
      else {
        assert(nextStartPos.line > startPos.line, s"Problem at $startPos with $x. Failed ${nextStartPos.line} > ${startPos.line}")
        rv += "\n"*(nextStartPos.line - startPos.line) + " "*nextStartPos.coln
      }

      x match {
        case x:BrackExpr => {
          var pos = span.left
          if(x.brackType != BracketType.fileToplevel) // in which case we don't use any brackets
            rv += bracketTypeToLeftGrouper(x.brackType)
          else
            pos = pos.plusColn(-1)
          for(y <- x.ch)
            pos = _unparse(y, pos.plusColn(1))
          val nextEndPos = span.right
          if(nextEndPos.line == pos.line) {
            assert(nextEndPos.coln >= pos.coln)
            rv += " "*(nextEndPos.coln - pos.coln - 1)
          }
          else {
            assert(nextEndPos.line > pos.line)
            rv += "\n"*(nextEndPos.line - pos.line) + " "*nextEndPos.coln
          }
          if(x.brackType != BracketType.fileToplevel) // in which case we don't use any brackets
            rv += grouper_map(bracketTypeToLeftGrouper(x.brackType))
        }
        case x:LeafExpr =>  rv += x.toString

      }
      span.right
    }
    _unparse(e, LCPos(0,0))
    rv
  }

}
