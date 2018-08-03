import miniL4.ast.Statement

package object miniL4 {
  val CONTRACT_ROLE = 'Contract
  type Nat = Int
  type Name = Symbol
  type Real = Double
  type TMap[K,V] = scala.collection.Map[K,V]
  type TSet[V] = scala.collection.Set[V]
  type Block = Seq[Statement]

  def warn(test:Boolean, msg:String) : Unit = println("WARNING: " + msg)
}
