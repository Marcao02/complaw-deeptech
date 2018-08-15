package miniL4.typechecker

import indy.TransitivelyClosedDirectedGraph
import miniL4.{Name, TMap, TSet}
import miniL4.ast.Datatype

class SubtypingGraph(dtypes:TSet[Datatype], seedSubtypePairs: TSet[(Datatype,Datatype)]) {
  private val graph = new TransitivelyClosedDirectedGraph[Datatype]

  graph.addNodes(dtypes)
  graph.addEdges(seedSubtypePairs)

//  println("The subtype graph seed:")
//  for(pair <- seedSubtypePairs)
//    println(pair)
//  println("The subtype graph:")
//  for(pair <- graph.edges)
//    println(pair)

  def subtype(x:Datatype,y:Datatype) : Boolean = graph.hasEdge(x,y)

  def simplifyIntersection(parts:List[Datatype]) : Datatype = {
    assert(parts.nonEmpty)
    if(parts.length == 1)
      parts.head
    else {
      var reduced_set = parts.toSet
      println("simplifying", reduced_set)
      for(u <- parts) {
        for(v <- parts) {
          if( u != v && reduced_set.contains(v) ) {
            if( subtype(u,v) )
              reduced_set = reduced_set - v
          }
        }
      }
      if(reduced_set.size == 1) {
        println(s"the set is reduced to ${reduced_set.head}")
        reduced_set.head
      }
      else
        throw new Exception(s"The set ${reduced_set} does not contain its lower bound.")
    }
  }
}
