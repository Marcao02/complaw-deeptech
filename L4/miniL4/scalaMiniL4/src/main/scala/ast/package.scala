
package object ast {
  type Nat = Int
  type Name = Symbol
  type Real = Double

  type Block = Seq[Statement]

  type TMap[K,V] = scala.collection.Map[K,V]
  type TSet[V] = scala.collection.Set[V]
}

