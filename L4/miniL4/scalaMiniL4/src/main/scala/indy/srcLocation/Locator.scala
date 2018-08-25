package indy.srcLocation

import indy.srcLocation.Loc.{LinPos, LinSpan}

class Locator(val filePath: String, val lineStartFileIndices: IndexedSeq[Int]) {
  def linPosToLCPos(pos:LinPos) : LCPos = {
    var left = 0
    var right = lineStartFileIndices.length - 1

    while(true) {
      val mid = Math.floor((left + right) / 2).toInt
      val line_mid_start_pos = lineStartFileIndices(mid)
      if(pos < line_mid_start_pos)
        right = mid - 1
      else if( mid < lineStartFileIndices.length - 1 && pos >= lineStartFileIndices(mid + 1))
        left = mid + 1
      else
        return LCPos(mid, pos - line_mid_start_pos)
    }

    throw new Exception("impossible?")
  }

  def linSpanToLCSpan(span:LinSpan) : LCSpan = LCSpan(linPosToLCPos(span._1), linPosToLCPos(span._2))


  def linPosToHeavySrcPos(pos:LinPos) : HeavySrcPos = HeavySrcPos(filePath, linPosToLCPos(pos))
  def linSpanToHeavySrcSpan(span:LinSpan) : HeavySrcSpan = HeavySrcSpan(filePath, linSpanToLCSpan(span))

}

