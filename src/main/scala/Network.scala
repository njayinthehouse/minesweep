import Network.Graph.Router
import smt.Z3.Sym.of
import smt.{ToZ3, Z3}
import smt.Z3._

object Network {

  type VertexId = Int

  def createMask(length: Int): String = {
    val a = for(_ <- 0 until length) yield "1"
    val b = for (_ <- length until 32) yield "0"
    "#b" + a.reduce((x, y) => x + y) + b.reduce((x, y) => x + y)
  }

  case class Graph(vs: Set[Graph.Vertex],
                   es: Set[(VertexId, VertexId)],
                   edge2Import: Map[(VertexId, VertexId), Filter],
                   edge2Export: Map[(VertexId, VertexId), Filter],
                   edge2Front: Map[(VertexId, VertexId), String],
                   edge2Back: Map[(VertexId, VertexId), String]) extends ToZ3[Prog[Decl]] {

    private def BEST(name: Int, protocol: Protocol): String = s"r${name}_best_$protocol"

      private def best(vName: Int, protocol: Protocol): Prog[Decl] =
        CreateSym(s"${vName}_best_$protocol", Z3.CprSort)+:
          Assert(Or(for {
            ((uName, _), cprName) <- edge2Front.toSeq
            if vName == uName
          } yield BEST(vName, protocol) =? cprName)) +:
          (for {
            ((uName, _), cprName) <- edge2Front
            if vName == uName
          } yield Assert(Preferred(BEST(vName, protocol), cprName))).toSeq


      override def toZ3: Prog[Decl] =
        (vs.map(_.toZ3)).toSeq ++
        (edge2Import.map(_._2.toZ3)).toSeq ++
        (edge2Export.map(_._2.toZ3)).toSeq
    }
  
  object Graph {

    abstract class Vertex extends ToZ3[Decl] {
      val name: VertexId

      override def toString: String = this match {
        case Router(_, _, protocol) => s"R${name}_$protocol"
        case _: Neighbor => s"N$name"
        case _: Subnet => s"S$name"
      }

      override def toZ3: Decl = CreateSym(this.toString, IntSort)
    }

    case class Router(name: VertexId, ip: Ip, protocol: Protocol) extends Vertex
    case class Neighbor(name: VertexId, ip: Ip) extends Vertex
    case class Subnet(name: VertexId, prefix: IpPrefix) extends Vertex
  }



  object ControlPlaneRecord extends ToZ3[Sort] {
    override def toZ3: Sort = Z3.CprSort

    object Declaration extends ToZ3[Decl] {
      override def toZ3: Decl = CreateCprSort
    }
  }

  case class Filter(cmd: Z3.Expr) extends ToZ3[Assert] {
    override def toZ3: Assert = Assert(cmd)
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
    {
      val FBM_test = Seq(
        CreateCprSort,
        CreateSym("c", Z3.CprSort),
        Assert(FBM(CprProj("c", Z3.Prefix), CprProj("c", Z3.Prefix), 5)),
        Z3.Sat,
        Z3.Model
      )
      for (i <- FBM_test) println(i)
      println("=======================================================================")
    }

    /*
     * Testing Graph
     */
    {
      val r1: Graph.Vertex = Router(1, Ip(12345), BGP)
      val r2: Graph.Vertex = Router(2, Ip(54321), BGP)
      val edge = (1, 2)
      val rec1 = "rec1"
      val rec2 = "rec2"
      val inFilter = Filter(Eq(CprProj(Sym(rec1), Z3.Valid), Bool(true)))
      val exFilter = Filter(Eq(CprProj(Sym(rec2), Z3.Valid), Bool(true)))

      val vertices = Set(r1, r2)
      val edges = Set(edge)
      val edge2Import = Map((edge -> inFilter))
      val edge2Export = Map((edge -> exFilter))
      val edge2Front = Map((edge -> rec1))
      val edge2Back = Map((edge -> rec2))

      val graph = Graph(vertices, edges, edge2Import, edge2Export, edge2Front, edge2Back)

      val GRAPH_test = Seq(
        CreateCprSort,
        CreateSym("rec1", Z3.CprSort),
        CreateSym("rec2", Z3.CprSort)
      ) ++
        graph.toZ3.ss ++
        Seq(
          Z3.Sat,
          Z3.Model
        )
      for (i <- GRAPH_test) println(i)
      println("=======================================================================")
    }

  }

  case class Port(port: Int) extends ToZ3[Z3.Num] {
    override def toZ3: Z3.Num = Z3.Num(port)
  }

  trait Protocol
  case object OSPF extends Protocol
  case object BGP extends Protocol
  case object CON extends Protocol
}