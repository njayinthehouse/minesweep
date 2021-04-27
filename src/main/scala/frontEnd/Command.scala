import Network.Graph.Vertex
import Network.IpPrefix

package frontEnd {

  abstract class Command
  case class AddEdge(src: Vertex, dst: Vertex) extends Command
  case class Deny(vertex: Vertex, prefix: IpPrefix, mask: Int, range: (Int, Int)) extends Command
  case class SetLocalPreference(vertex: Vertex, pref: Int) extends Command

  trait Property extends smt.ToZ3
}