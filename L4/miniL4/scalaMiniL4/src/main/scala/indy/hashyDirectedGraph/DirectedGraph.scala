package indy.hashyDirectedGraph

import indy.type_abbrevs.TSet

sealed abstract class GraphTraversalResult[T]
case class CycleInvolving[T](involving:T) extends GraphTraversalResult[T]
case class TraversalNoCycleFound[T](seq:Seq[T], unvisited:TSet[T]) extends GraphTraversalResult[T]
case class NoSourceNodes[T](insubset:TSet[T]) extends GraphTraversalResult[T]


trait DirectedGraph[T] {
  def nodes : Iterable[T]
  def edges : TSet[(T,T)]

  def hasNode(node:T) : Boolean
  def hasEdge(u1:T,u2:T) : Boolean

  def addNode(u:T): Unit
  def addEdge(src: T, trg: T): Unit

  def addNodes(nodes: Iterable[T]): Unit
  def addEdges(edges: Iterable[(T, T)]): Unit

  def dfs(startNode: T, start_unvisited: TSet[T]): GraphTraversalResult[T]

  def topSortOrEvidenceOfNone(): GraphTraversalResult[T]
}
