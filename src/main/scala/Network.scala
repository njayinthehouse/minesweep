import Network.Graph.{Edge, Router}
import smt.{ToZ3, Z3}
import smt.Z3.{Assert, BitVecSort, CreateCprSort, CreateSym, Decl, Hex, IntSort, Prog, Sort}

object Network {

  def createMask(length: Int): String = {
    val a = for(_ <- 0 until length) yield "1"
    val b = for (_ <- length until 32) yield "0"
    "#b" + a.reduce((x, y) => x + y) + b.reduce((x, y) => x + y)
  }

  /*
  type Graph = graph.Graph[(ControlPlaneRec, ControlPlaneRec)]
  type Vertex = graph.Graph.Vertex
  type Edge = graph.Graph.Edge[(ControlPlaneRec, ControlPlaneRec)]
*/

  case class Graph(vs: Set[Graph.Vertex], es: Set[Graph.Edge]) extends ToZ3[Prog[Decl]] {
    override def toZ3: Prog[Decl] = vs.toSeq.map(_.toZ3) ++ es.toSeq.flatMap(_.toZ3.ss)
  }

  object Graph {

    case class Edge(v: Vertex, w: Vertex,
                    front: String, back: String,
                    inFilter: Option[Filter], outFilter: Option[Filter])
      extends ToZ3[Prog[Assert]] {

      override def toZ3: Prog[Assert] = inFilter.toSeq.map(_.toZ3) ++ outFilter.toSeq.map(_.toZ3)
    }

    abstract class Vertex extends ToZ3[Decl] {
      val name: String

      override def toZ3: Decl = CreateSym(name, IntSort)
    }

    case class Router(name: String, ip: Ip, protocol: Protocol) extends Vertex
    case class Neighbor(name: String, ip: Ip) extends Vertex
    // TODO: should we keep it or just use Neighbor only?
    case class Subnet(name: String, prefix: IpPrefix) extends Vertex

  }

  object ControlPlaneRecord extends ToZ3[Sort] {
    override def toZ3: Sort = Z3.CprSort

    object Declaration extends ToZ3[Decl] {
      override def toZ3: Decl = CreateCprSort
    }
  }

  case class Filter(cmd: MS) extends ToZ3[Assert] {
    override def toZ3: Assert = Assert(cmd.toZ3)
  }

  case class IpPrefix(prefix: Int, length: Int) extends ToZ3[Hex] {
    def toZ3: Hex = Hex("#x" ++ prefix.toHexString.reverse.padTo(8, '0').reverse)
  }

  case class Ip(prefix: Int) extends ToZ3[Hex] {
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
      ) ++
        srcIp.map(x => Assert(Z3.Eq(x.toZ3, Z3.Sym("srcIp")))).toSeq ++
        srcPort.map(x => Assert(Z3.Eq(x.toZ3, Z3.Sym("srcPort")))).toSeq ++
        dstIp.map(x => Assert(Z3.Eq(x.toZ3, Z3.Sym("dstIp")))).toSeq ++
        dstPort.map(x => Assert(Z3.Eq(x.toZ3, Z3.Sym("dstPort")))).toSeq
  }


  // Temporary main for testing.
  def main(args: Array[String]): Unit = {
    /*
     * Testing FBM (reflexivity)
     */
    val FBM_test = Seq(
      CreateCprSort,
      CreateSym("c", Z3.CprSort),
      Assert(FBM(Proj(Var("c"), Z3.Prefix), Proj(Var("c"), Z3.Prefix), Num(5)).toZ3),
      Z3.Sat,
      Z3.Model
    )
    for (i <- FBM_test) println(i)

    println("=======================================================================")

    /*
     * Testing Graph
     */
    val v1 = Router("r1", Ip(12345), BGP)
    val v2 = Router("r2", Ip(54321), BGP)
    val edge = Edge(v1, v2, "rec1", "rec2", None, None)
    val packet = Packet(Some(Ip(1111)), Some(Port(1)), Some(Ip(2222)), Some(Port(22)))
    val graph = Graph(Set[Network.Graph.Vertex](v1, v2), Set[Edge](edge))

    val GRAPH_test = Seq(
      CreateCprSort,
      CreateSym("rec1", Z3.CprSort),
      CreateSym("rec2", Z3.CprSort)
    ) ++
      packet.toZ3.ss ++
      graph.toZ3.ss ++
      Seq(
        Z3.Sat,
        Z3.Model
      )
    for (i <- GRAPH_test) println(i)
  }

  case class Port(port: Int) extends ToZ3[Z3.Num] {
    override def toZ3: Z3.Num = Z3.Num(port)
  }

  trait Protocol
  case object OSPF extends Protocol
  case object BGP extends Protocol
  case object CON extends Protocol
}