import Network.{IpPrefix, Vertex}
import smt.{Z3, ToZ3}

package frontEnd {


  abstract class Command
  case class AddEdge(src: Vertex, dst: Vertex) extends Command
  case class Deny(vertex: Vertex, prefix: IpPrefix, mask: Int, range: (Int, Int)) extends Command
  case class SetLocalPreference(vertex: Vertex, pref: Int) extends Command

  trait Property extends ToZ3[Z3.Stmt]
}