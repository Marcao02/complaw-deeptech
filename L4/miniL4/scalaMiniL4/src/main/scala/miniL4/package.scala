import miniL4.ast.Statement

package object miniL4 {
  val CONTRACT_ROLE = 'Contract
  type Nat = Int
  type Name = Symbol
  type Real = Double
  type TMap[K,V] = scala.collection.Map[K,V]
  type TSet[V] = scala.collection.Set[V]
  type Block = Seq[Statement]

  def warn(test:Boolean, msg:String) : Unit = if(test) println("WARNING: " + msg) else ()

  def seqmapHasKey[K,V](seq:Seq[(K,V)], k:K) : Boolean = seq.exists({case (k2,_) => k == k2})
  def seqmapGet[K,V](seq:Seq[(K,V)], k:K) : V = {
    for (pair <- seq) {
      if( pair._1 == k )
        return pair._2
    }
    throw new Exception(s"key not found: $k")
  }

  def stringJoin(seq:Seq[String], delim:String) = seq.reduce((x,y) => s"$x${delim}$y")
  def mapToStringJoin[T](seq:Iterable[T], delim:String, fn: T => String) = seq.view.map(fn).reduce((x,y) => s"$x${delim}$y")

}
