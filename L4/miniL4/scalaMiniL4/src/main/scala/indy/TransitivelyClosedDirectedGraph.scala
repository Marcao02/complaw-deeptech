package indy

import miniL4.TSet

import scala.collection.mutable.{HashMap, HashSet}


/*
NOTE: addEdge makes use of fact that graph has self-edges
*/
class TransitivelyClosedDirectedGraph[T] {
  private val edges_from = HashMap.empty[T,HashSet[T]]
  private val edges_to = HashMap.empty[T,HashSet[T]]
  private val all_edges = HashSet.empty[(T,T)]
  def nodes = edges_from.keys
  def edges : TSet[(T,T)] = all_edges

  def hasNode(node:T) : Boolean = edges_from.contains(node)
  def hasEdge(u1:T,u2:T) : Boolean = edges_from(u1).contains(u2)

  def addNode(u:T): Unit = {
    if (!edges_from.contains(u)) {
      edges_from.update(u, HashSet[T](u))
      assert(!edges_to.contains(u))
      edges_to.update(u, HashSet[T](u))
      all_edges.add((u,u))
    }
  }

  def addEdge(src:T, trg:T): Unit = {
    if(!hasNode(src)) addNode(src)
    if(!hasNode(trg)) addNode(trg)
    if(hasEdge(src,trg)) // it was added earlier
      return

    all_edges.add((src,trg))
    edges_from(src).add(trg)
    edges_to(trg).add(src)

    for(ancestor_of_trg <- edges_from(trg)) {
      for (descendent_of_src <- edges_to(src)) {
        all_edges.add((descendent_of_src, ancestor_of_trg))
        edges_from(descendent_of_src).add(ancestor_of_trg)
        edges_to(ancestor_of_trg).add(descendent_of_src)
      }
    }
  }

  def addNodes(nodes:Iterable[T]): Unit = nodes.foreach(addNode(_))
  def addEdges(edges:Iterable[(T,T)]): Unit = for((x,y) <- edges) { addEdge(x,y) }
}
