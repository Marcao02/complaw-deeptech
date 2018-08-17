package indy

object type_abbrevs {
  type Nat = Int
  type Name = Symbol
  type Real = Double
  type TMap[K,V] = scala.collection.Map[K,V]
  type TSet[X] = scala.collection.Set[X] // `Set` on its own defaults to scala.collection.immutable.Set

}
