
package object ast {
  type Nat = Int
  type Name = Symbol
  type Real = Double

  type Block = Seq[Statement]

  type IMap[K,V] = scala.collection.Map[K,V]
}

