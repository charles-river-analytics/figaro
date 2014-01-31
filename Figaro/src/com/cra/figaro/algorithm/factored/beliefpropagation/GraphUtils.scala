package com.cra.figaro.algorithm.factored.beliefpropagation

/*
 * Utilities for working with undirected graphs
 */
object GraphUtils {

  type Graph[T] = Map[T, Set[T]]

  def leaves[T](graph: Graph[T]) = graph.filter(_._2.size == 1).keys.toSet

  def neighbors[T](graph: Graph[T], source: T) = graph(source)

  def neighbors[T](graph: Graph[T], source: T, excluding: T) =
    for (node <- graph(source) if node != excluding) yield node

  def containsLoop[T](graph: Graph[T], start: T) = {
    def visit(source: T, neighboring: Iterable[T], visited: collection.mutable.Set[T]): Boolean = {
      if (!visited.add(source))
        return false

      for (node <- neighboring) {
        if (!visit(node, neighbors(graph, node, source), visited))
          return false;
      }

      true;
    }

    !visit(start, graph(start), collection.mutable.Set[T]())
  }

}