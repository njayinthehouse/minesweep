package graph {
  import smt.Z3.T

  case class Graph[E](vs: Set[Graph.Vertex], es: Set[Graph.Edge[E]]) extends smt.ToZ3 {
    override def toZ3: T = es flatMap toZ3
  }

  object Graph {

    case class Edge[E](v: Vertex, w: Vertex, info: E) extends smt.ToZ3 {
      override def toZ3: T =
    }

    trait Vertex
  }

}