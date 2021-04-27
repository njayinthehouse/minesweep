import smt.{Z3, ToZ3}
import smt.Z3.{CreateSym, CreateEqual, Hex, Stmts, Sym}

object Network {
  trait Address
/*
  type Graph = graph.Graph[(ControlPlaneRec, ControlPlaneRec)]
  type Vertex = graph.Graph.Vertex
  type Edge = graph.Graph.Edge[(ControlPlaneRec, ControlPlaneRec)]
*/
  case class Graph(vs: Set[Graph.Vertex], es: Set[Graph.Edge]) extends ToZ3[Stmts] {
    override def toZ3: Z3.Stmts = Stmts(es.map(_.toZ3).toSeq)
  }

  object Graph {

    case class Edge(v: Vertex, w: Vertex,
                    front: ControlPlaneRec, back: ControlPlaneRec,
                    inFilter: Filter, outFilter: Filter)
      extends ToZ3[Stmts] {

      override def toZ3: Z3.Stmts = inFilter.toZ3 ++ outFilter.toZ3
    }

    trait Vertex
  }

  // Routers are subrouters running a protocol
  case class Router(name: String, ip: Ip, protocol: Protocol) extends Graph.Vertex
  case class Neighbor(name: String, ip: Ip) extends Graph.Vertex
  // Chose not to put a graph in subnet, since we're abstracting over that
  case class Subnet(name: String, prefix: IpPrefix) extends Graph.Vertex

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

  trait Filter extends ToZ3[Stmts]

  case class Ip(ip: Int) extends ToZ3[Hex] {
    def toZ3: Hex = Z3.Hex("#x" ++ ip.toHexString.reverse.padTo(8, '0').reverse)
  }
  object Ip {
    val length: Int = 32
  }

  trait IpPrefix {
    val prefix: Int
  }

  type Metric = Int
  type MED = Int

  case class Packet(srcIp: Option[Ip], srcPort: Option[Port], dstIp: Option[Ip], dstPort: Option[Port])
    extends ToZ3[Z3.Stmt] {
    override def toZ3: Z3.Stmt = {
      CreateSym("srcIp", Z3.BitVecSort(Ip.length)) ++
        CreateSym("srcPort", Z3.IntS) ++
        CreateSym("dstIp", Z3.BitVecSort(Ip.length)) ++
        CreateSym("dstPort", Z3.IntS) ++
        srcIp.map(ip => CreateEqual(Sym("srcIp"), ip.toZ3)) ++
        srcPort.map(port => CreateEqual(Sym("srcPort"), port.toZ3)) ++
        dstIp.map(ip => CreateEqual(Sym("dstIp"), ip.toZ3)) ++
        dstPort.map(port => CreateEqual(Sym("dstPort"), port toZ3))
    }
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
