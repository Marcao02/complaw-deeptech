package indy
import type_abbrevs._

object util {
  def warn(test:Boolean, msg:String) : Unit = if(test) println("WARNING: " + msg) else ()

  // TODO: move everything below here to package `indy`
  def seqToPairs[V](seq:Seq[V]) : Iterable[(V,V)] = {
    seq.indices.init.map( (i:Int) => (seq(i),seq(i+1)) )
  }

  def seqmapHasKey[K,V](seq:Seq[(K,V)], k:K) : Boolean = seq.exists({case (k2,_) => k == k2})
  def seqmapGet[K,V](seq:Seq[(K,V)], k:K) : V = {
    for (pair <- seq) {
      if( pair._1 == k )
        return pair._2
    }
    throw new Exception(s"key not found: $k")
  }

  //  def stringJoin[T](seq:Seq[T], delim:String): String = seq.reduce[T]((x:T,y:T) => s"${x.toString}${delim}${y.toString}")
//  def stringJoin(seq:Iterable[String], delim:String): String = if(seq.isEmpty) "" (else seq.reduce((x,y) => s"$x$delim$y")
  def stringJoin(seq:Iterable[String], delim:String): String = seq.mkString("",delim,"")
//{
//    println(seq)
//    val s = seq.toIndexedSeq
//    if(s.isEmpty) ""
//    else {
//      if(s.length == 1) s(0)
//      else s.reduce((x,y) => s"$x$delim$y")
//    }
//  }
  def mapToStringJoin[T](seq:Iterable[T], delim:String, fn: T => String): String = stringJoin(seq.view.map(fn),delim)
  def toStringJoin[T](seq:Iterable[T], delim:String): String = stringJoin(seq.map(_.toString), delim)

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
