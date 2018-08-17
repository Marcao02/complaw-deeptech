package indy.hashyDirectedGraph

import miniL4.TSet

import scala.collection.mutable
import scala.collection.mutable.{HashMap, HashSet}


class HashyDirectedGraph[T] extends DirectedGraph[T] {
  private val edges_from = HashMap.empty[T, HashSet[T]]
  private val edges_to = HashMap.empty[T, HashSet[T]]
  private val all_edges = HashSet.empty[(T, T)]

  // TODO temp till I move these into indy
  def stringJoin(seq:Iterable[String], delim:String): String = if(seq.nonEmpty) seq.reduce((x,y) => s"$x$delim$y") else ""
  def mapToStringJoin[T](seq:Iterable[T], delim:String, fn: T => String): String = stringJoin(seq.view.map(fn),delim)
  def toStringJoin[T](seq:Iterable[T], delim:String): String = stringJoin(seq.map(_.toString), delim)
  override def toString: String = toStringJoin(all_edges, "\n")

  def nodes = edges_from.keys.toSet
  def edges: TSet[(T, T)] = all_edges

  def hasNode(node: T): Boolean = edges_from.contains(node)
  def hasEdge(u1: T, u2: T): Boolean = edges_from(u1).contains(u2)

  def addNode(u: T): Unit = {
    if (!edges_from.contains(u)) {
      edges_from.update(u, HashSet.empty[T])
      assert(!edges_to.contains(u))
      edges_to.update(u, HashSet.empty[T])
    }
  }

  def addEdge(src: T, trg: T): Unit = {
    if (!hasNode(src)) addNode(src)
    if (!hasNode(trg)) addNode(trg)
    if (hasEdge(src, trg))
      return

    all_edges.add((src, trg))
    edges_from(src).add(trg)
    edges_to(trg).add(src)
  }

  def addNodes(nodes: Iterable[T]): Unit = for (x <- nodes) addNode(x)
  def addEdges(edges: Iterable[(T, T)]): Unit = for ((x, y) <- edges) addEdge(x, y)

  def dfs(startNode: T, start_unvisited: TSet[T]): GraphTraversalResult[T] = {
    var unvisited = start_unvisited
    // mutability of the following three does not escape function boundary
    val landed = HashSet.empty[T]
    val tapped = HashSet.empty[T]
    var seq = mutable.Seq.empty[T]

    tapped.add(startNode)
    unvisited = unvisited - startNode
    var cur = startNode

    while (tapped.nonEmpty) {
      cur = tapped.head
      unvisited = unvisited - cur
      seq = seq :+ cur
      landed.add(cur)
      tapped.remove(cur)
      for (v <- edges_from(cur)) {
        if (landed.contains(v))
          return CycleInvolving(v)
        else /* if(!tapped.contains(v)) */
          tapped.add(v) // since tapped is a set, don't need to do the contains check
      }
    }
    TraversalNoCycleFound(seq, unvisited)
  }

  def topSortOrEvidenceOfNone(): GraphTraversalResult[T] = {
    var nodesleft : TSet[T] = nodes.toSet
    var path = Seq.empty[T]
    var maybeSource : Option[T] = nodesleft.find( u => edges_from(u).isEmpty )
    while(true) {
      println(s"maybeSource: ${maybeSource}. nodesleft ${nodesleft}.")
      maybeSource match {
        case Some(src) => dfs(src, nodesleft) match {
          case TraversalNoCycleFound(morepath, unvisited) => {
            path = path ++ morepath
            nodesleft = unvisited
            maybeSource = nodesleft.find( u => edges_from(u).isEmpty )
          }
          case c@CycleInvolving(_) => return c
          case NoSourceNodes(_) => throw new Exception("can't get here")
        }
        case None => {
          if(nodesleft.isEmpty)  return TraversalNoCycleFound(path, nodesleft)
          else  return NoSourceNodes(nodesleft)
        }
      }
    }
    throw new Exception("can't get here")
  }
}