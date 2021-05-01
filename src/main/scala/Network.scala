import smt.Z3.Sym.of
import smt.{ToZ3, Z3}
import smt.Z3._

object Network {

  // Best variable
  case class Best(routerName: VertexId, protocol: Protocol) extends ToZ3[Sym] {
    override def toZ3: Sym = this.toString

    override def toString: String = s"r${routerName}_best_$protocol"

    def declaration(cprNames: Seq[String]): ToZ3[Prog[Decl]] = new ToZ3[Prog[Decl]] {
      override def toZ3: Prog[Decl] =
        CreateSym(Best.this.toString, CprSort) +:
          Assert(bestIsOne) +:
          bestIsPreferred.map(Assert(_))

      val bestIsPreferred: Seq[Or] = cprNames.map(Preferred(Best.this.toString, _))
      val bestIsOne: Or = Or(cprNames.map(Best.this.toString =? _))
    }
  }

  def createMask(length: Int): String = {
    val a = for(_ <- 0 until length) yield "1"
    val b = for (_ <- length until 32) yield "0"
    "#b" + a.reduce((x, y) => x + y) + b.reduce((x, y) => x + y)
  }

  def ControlFwd(r1: VertexId, r2: VertexId): Sym = Sym(s"ControlFwd_${r1}_$r2")

  object ControlPlaneRecord extends ToZ3[Sort] {
    override def toZ3: Sort = Z3.CprSort

    object Declaration extends ToZ3[Decl] {
      override def toZ3: Decl = CreateCprSort
    }
  }

  def DataFwd(r1: VertexId, r2: VertexId): Sym = Sym(s"DataFwd_${r1}_$r2")

  type Filter = Z3.Expr

  case class Graph(vs: Set[Vertex],
                   es: Set[(VertexId, VertexId)],
                   edge2Import: Map[(VertexId, VertexId), Filter],
                   edge2Export: Map[(VertexId, VertexId), Filter],
                   edge2Front: Map[(VertexId, VertexId), String],
                   edge2Back: Map[(VertexId, VertexId), String]) {

    val declaration: ToZ3[Prog[Decl]] = new ToZ3[Prog[Decl]] {
      def toZ3: Prog[Decl] =
        vs.map(_.declaration.toZ3).toSeq ++                                // Vertices
          edge2Import.values.map(Assert) ++                                // Import filters
          edge2Export.values.map(Assert) ++                                // Export filters
          bests.flatMap(_.declaration(bgpRouterNames.toSeq).toZ3)          // Bests

      // We only check best for BGP
      val bgpRouters: Set[Vertex] = vs.filter { case Router(_, _, BGP) => true }
      val bgpRouterNames: Set[String] = bgpRouters.map(_.toString)
      val bests: Set[Best] = bgpRouters.map(r => Best(r.name, BGP))
    }
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

  case class Port(port: Int) extends ToZ3[Num] {
    override def toZ3: Num = Num(port)
  }

  trait Protocol
  case object OSPF extends Protocol
  case object BGP extends Protocol
  case object CON extends Protocol

  // Vertices can be routers, external neighbors or subnets
  case class Router(name: VertexId, ip: Ip, protocol: Protocol) extends Vertex
  case class Neighbor(name: VertexId, ip: Ip) extends Vertex
  case class Subnet(name: VertexId, prefix: IpPrefix) extends Vertex

  abstract class Vertex extends ToZ3[Sym] {
    val name: VertexId

    override def toZ3: Sym = this.toString

    override def toString: String = this match {
      case Router(_, _, protocol) => s"R${name}_$protocol"
      case _: Neighbor => s"N$name"
      case _: Subnet => s"S$name"
    }

    val declaration: ToZ3[CreateSym] = new ToZ3[CreateSym] {
      override def toZ3: CreateSym = CreateSym(Vertex.this.toZ3, IntSort)
    }
  }

  type VertexId = Int

<<<<<<< HEAD
  // Temporary main for testing.
  def main(args: Array[String]): Unit = {
    def FBM_reflexivity(): Unit = {
      val FBM_test = Seq(
        CreateCprSort,
        CreateSym("c", Z3.CprSort),
        Assert(FBM(CprProj("c", Z3.Prefix), CprProj("c", Z3.Prefix), 5)),
        Z3.Sat,
        Z3.Model
      )
      for (i <- FBM_test) println(i)
    }

    def graph1(): Unit = {
      val r1: Vertex = Router(1, Ip(12345), BGP)
      val r2: Vertex = Router(2, Ip(54321), BGP)
      val edge = (1, 2)
      val rec1 = "rec1"
      val rec2 = "rec2"
      val inFilter = CprProj(Sym(rec1), Z3.Valid) =? Bool(true)
      val exFilter = CprProj(Sym(rec2), Z3.Valid) =? Bool(true)

      val vertices = Set(r1, r2)
      val edges = Set(edge)
      val edge2Import = Map(edge -> inFilter)
      val edge2Export = Map(edge -> exFilter)
      val edge2Front = Map(edge -> rec1)
      val edge2Back = Map(edge -> rec2)

      val graph = Graph(vertices, edges, edge2Import, edge2Export, edge2Front, edge2Back)

      val GRAPH_test = Seq(
        CreateCprSort,
        CreateSym("rec1", Z3.CprSort),
        CreateSym("rec2", Z3.CprSort)
      ) ++
        graph.declaration.toZ3 ++
        Seq(
          Z3.Sat,
          Z3.Model
        )
      for (i <- GRAPH_test) println(i)
    }

    def best1(): Unit = {
      val best = Best(1, BGP)
      println(best.declaration(Seq("x", "y")).toZ3)
    }

    graph1()
  }
}
