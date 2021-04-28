import smt.{ToZ3, Z3}
import smt.Z3.{Assert, BitVecSort, CreateCprSort, CreateSym, Decl, Hex, IntSort, Prog, Sort}

object Network {

  trait Address

  /*
  type Graph = graph.Graph[(ControlPlaneRec, ControlPlaneRec)]
  type Vertex = graph.Graph.Vertex
  type Edge = graph.Graph.Edge[(ControlPlaneRec, ControlPlaneRec)]
*/
  case class Graph(vs: Set[Graph.Vertex]) extends ToZ3[Prog[Decl]] {
    override def toZ3: Prog[Decl] = vs.toSeq.flatMap(_.toZ3.ss)
  }

  object Graph {

    case class Edge(v: Vertex, w: Vertex,
                    front: ControlPlaneRecord, back: ControlPlaneRecord,
                    inFilter: Option[Filter], outFilter: Option[Filter])
      extends ToZ3[Prog[Assert]] {

      override def toZ3: Prog[Assert] = inFilter.toSeq.map(_.toZ3) ++ outFilter.toSeq.map(_.toZ3)
    }

    abstract class Vertex extends ToZ3[Prog[Decl]] {
      val incoming: Seq[Edge] = ???
      val outgoing: Seq[Edge]
      val name: String

      override def toZ3: Prog[Decl] = outgoing.flatMap(_.toZ3.ss) :+ CreateSym(name, IntSort)
    }

    case class Router(name: String, outgoing: Seq[Edge], ip: Ip, protocol: Protocol) extends Vertex
    case class Neighbor(name: String, outgoing: Seq[Edge], ip: Ip) extends Vertex
    // TODO: should we keep it or just use Neighbor only?
    case class Subnet(name: String, outgoing: Seq[Edge], prefix: IpPrefix) extends Vertex

  }

  case class ControlPlaneRecord
  (prefix: IpPrefix,
   /* length: Int, */
   address: Address,
   lp: Ip,
   metric: Metric,
   med: MED, // Multiexit discriminator
   rid: Ip,
   bgpInternal: Boolean,
   valid: Boolean) {
  }

  object ControlPlaneRecord extends ToZ3[Sort] {
    override def toZ3: Sort = ???

    object Declaration extends ToZ3[Decl] {
      override def toZ3: Decl = CreateCprSort
    }
  }

  case class Filter(cmd: MS) extends ToZ3[Assert] {
    override def toZ3: Assert = Assert(cmd.toZ3)
  }

  trait IpPrefix {
    val prefix: Int
    val length: Int
  }

  object IpPrefix extends ToZ3[Sort] {
    override def toZ3: Sort = BitVecSort(32)
  }


  case class Ip(prefix: Int) extends ToZ3[Hex] with IpPrefix {
    val length = 32
    def toZ3: Hex = Hex("#x" ++ prefix.toHexString.reverse.padTo(8, '0').reverse)
  }

  object Ip extends ToZ3[Sort] {
    override def toZ3: Sort = BitVecSort(32)
  }

  type Metric = Int
  type MED = Int

  case class Packet(srcIp: Option[Ip], srcPort: Option[Port], dstIp: Option[Ip], dstPort: Option[Port])
    extends ToZ3[Prog[Decl]] {
    override def toZ3: Prog[Decl] = Seq (
      CreateSym("srcIp", Z3.BitVecSort(32)),
      CreateSym("srcPort", Z3.IntSort),
      CreateSym("dstIp", Z3.BitVecSort(32)),
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
