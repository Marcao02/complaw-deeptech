import com.github.nscala_time.time.Imports._
import com.github.nscala_time.time.Imports.{DateTime => DateTimeFromLib}
import org.joda.time.Period

import SortOcc.SortOcc
import Statement.Statement
import Term.Term
import org.joda.time.Period

import scala.collection.{SortedMap, SortedSet}

sealed abstract class Loc(filepath:String, line:Int, col:Int)
case class SrcLoc(filepath:String, line:Int, col:Int) extends Loc(filepath,line,col)
// For GeneratedSrcLoc, the line and col are approximate
case class GeneratedSrcLoc(filepath:String, line:Int, col:Int) extends Loc(filepath,line,col)
trait Locatable {
  val loc:Loc
}

class SyntaxError(val msg:String) extends Exception(msg)
class TypeCheckingError(val msg:String) extends Exception(msg)

object TypeAliases {
  type DateTime = DateTimeFromLib
  type TimeDelta = Period
  type TimeUnit = String
  type OrderedMap[K, V] = SortedMap[K, V]
  type OrderedSet[V] = SortedSet[V]

  type TermSeq = Seq[Term]
  type SortOccSeq = Seq[SortOcc]
  type Block = Seq[Statement]
}
