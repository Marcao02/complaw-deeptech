package miniL4.ast

import miniL4.Nat

sealed abstract class Loc {}
  case object NoLoc extends Loc
  sealed abstract class FileLoc(filepath:String) extends Loc
    case class MultilineSrcSpan(filepath:String, startLine:Nat, startCol:Nat, stopLine:Nat, stopCol:Nat) extends FileLoc(filepath)
    case class SrcLoc(filepath:String, line:Nat, col:Nat) extends FileLoc(filepath)
    case class GeneratedSrcLoc(filepath:String, line:Nat, col:Nat) extends FileLoc(filepath)

trait Locatable {
  val loc:Loc
}
