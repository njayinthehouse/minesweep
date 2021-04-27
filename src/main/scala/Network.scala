import smt.{ToZ3, Z3}
import smt.Z3.{CreateSym, Decl, Hex, Prog, Sort, Stmt, Sym}

object Network {

  trait Address

  /*
  type Graph = graph.Graph[(ControlPlaneRec, ControlPlaneRec)]
  type Vertex = graph.Graph.Vertex
  type Edge = graph.Graph.Edge[(ControlPlaneRec, ControlPlaneRec)]
*/
  case class Graph(vs: Set[Graph.Vertex], es: Set[Graph.Edge]) extends ToZ3[Prog[Decl]] {
    override def toZ3: Prog[Decl] = ???
  }

  object Graph {

    case class Edge(v: Vertex, w: Vertex,
                    front: ControlPlaneRec, back: ControlPlaneRec,
                    inFilter: Filter, outFilter: Filter)
      extends ToZ3[Prog[Decl]] {

      override def toZ3: Prog[Decl] = ???
    }

    abstract class Vertex extends ToZ3[Prog[Decl]] {
      override def toZ3: Prog[Decl] = ???
    }

    case class Router(name: String, ip: Ip, protocol: Protocol) extends Vertex

    case class Neighbor(name: String, ip: Ip) extends Vertex

    case class Subnet(name: String, prefix: IpPrefix) extends Vertex

  }

  case class ControlPlaneRec
  (prefix: IpPrefix,
   length: Int,
   address: Address,
   lp: Ip,
   metric: Metric,
   med: MED, // Multiexit discriminator
   rid: Ip,
   bgpInternal: Boolean,
   valid: Boolean)

  object ControlPlaneRec extends ToZ3[Z3.Decl] {
    def toZ3: Decl = ???
  }

  trait Filter extends ToZ3[Prog[Decl]]

  case class Ip(ip: Int) extends ToZ3[Hex] {
    def toZ3: Hex = Hex("#x" ++ ip.toHexString.reverse.padTo(8, '0').reverse)
  }

  object Ip extends ToZ3[Sort] {
    val length: Int = 32

    override def toZ3: Sort = ???
  }

  trait IpPrefix {
    val prefix: Int
  }

  type Metric = Int
  type MED = Int

  case class Packet(srcIp: Option[Ip], srcPort: Option[Port], dstIp: Option[Ip], dstPort: Option[Port])
    extends ToZ3[Prog[Decl]] {
    override def toZ3: Prog[Decl] = Seq (
      CreateSym("srcIp", Z3.BitVecSort(Ip.length)),
      CreateSym("srcPort", Z3.IntSort),
      CreateSym("dstIp", Z3.BitVecSort(Ip.length)),
      CreateSym("dstPort", Z3.IntSort)
    )
  }


  // Temporary main for testing.
  def main(args: Array[String]): Unit = print(Packet(Some(Ip(1)), Some(Port(2)), Some(Ip(3)), Some(Port(4))).toZ3)

  case class Port(port: Int) extends ToZ3[Z3.Num] {
    override def toZ3: Z3.Num = Z3.Num(port)
  }

  trait Protocol
  case object OSPF extends Protocol
  case object BGP extends Protocol
  case object CON extends Protocol
}
