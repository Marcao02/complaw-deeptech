//import miniL4.ast.Statement

package object miniL4 {
  val CONTRACT_ROLE = 'Contract
  type Nat = Int
  type Name = Symbol
  type Real = Double
  type TMap[K,V] = scala.collection.Map[K,V]
  type TSet[V] = scala.collection.Set[V]

  def warn(test:Boolean, msg:String) : Unit = if(test) println("WARNING: " + msg) else ()
  def bugassert(test:Boolean, msg:String) : Unit = if(!test) throw new BugInCodebase(msg) else ()

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

  // just a stub for a map-reduce-by-union (maximally parallelizable, in principle)
  def mapCollectAnyOrder[T,V](iter:Iterable[T], fn:T => Iterable[V]) : Iterable[V] = {
    iter.flatMap(fn)
  }

  def proj1[X,Y](iter:Iterable[(X,Y)]) : Iterable[X] = iter.map(_._1)

  def proj2[X,Y](iter:Iterable[(X,Y)]) : Iterable[Y] = iter.map(_._2)

  def proj1fromMap[K,V1,V2](m:TMap[K,(V1,V2)]) : TMap[K,V1] = m.mapValues(_._1)

  def proj2fromMap[K,V1,V2](m:TMap[K,(V1,V2)]) : TMap[K,V2] = m.mapValues(_._2)

  // OPT
  def projections[K,V1,V2](m:TMap[K,(V1,V2)]) : (TMap[K,V1],TMap[K,V2]) = (m.mapValues(_._1), m.mapValues(_._2))

}
