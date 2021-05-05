import smt.Z3.Sym.of
import smt.{ToZ3, Z3}
import smt.Z3._

import scala.collection.mutable.ListBuffer

object Network {

  // The expr is a BitVecSort in Z3
  case class AccessControlList(blacklist: Seq[IpPrefix]) extends ToZ3[Expr] {
    override def toZ3: Expr = Not(Or(blacklist.map(prefix => FBM(prefix.toZ3, Packet.dstIp.toZ3, prefix.length))))
  }

  // Best variable
  // TODO: Make sure best is valid -- is this a problem in the paper?
  // This is general enough for symbolic bests for any protocol, but we only define BGP
  case class Best(routerName: VertexId, protocol: Protocol) extends ToZ3[Sym] {
    override def toZ3: Sym = this.toString

    override def toString: String = s"r${routerName}_best_$protocol"

    def declaration(cprNames: Seq[String]): ToZ3[Prog[Decl]] = new ToZ3[Prog[Decl]] {
      override def toZ3: Prog[Decl] =
        CreateSym(Best.this.toString, CprSort) +:
          Assert(atLeastOneBest) +:
          bestIsPreferred.map(Assert(_))

      val bestIsPreferred: Seq[Or] = cprNames.map(Preferred(Best.this.toString, _))
      val atLeastOneBest: Or = Or(cprNames.map(Best.this.toString =? _))
    }
  }

  case class CanReach(r1: VertexId) extends ToZ3[Sym] {
    override def toZ3: Sym = this.toString

    override def toString: CprId = s"canReach_$r1"

    val declaration = ???
  }

  case class ControlFwd(r1: VertexId, r2: VertexId) extends ToZ3[Sym] {
    override def toZ3: Sym = this.toString

    override def toString: String = s"controlfwd_${r1}_$r2"

    def declaration(cprName: String): ToZ3[Prog[Decl]] = new ToZ3[Prog[Decl]] {
      override def toZ3: Prog[Decl] = Seq(
        CreateSym(ControlFwd.this.toString, BoolSort),
        Assert(ControlFwd.this.toZ3 =? (CprProj(cprName, Valid) && (cprName =? Best(r1, BGP).toZ3)))
      )
    }
  }

  type CprId = String

  def createMask(length: Int): String = {
    val a = for(_ <- 0 until length) yield "1"
    val b = for (_ <- length until 32) yield "0"
    "#b" + a.reduce((x, y) => x + y) + b.reduce((x, y) => x + y)
  }

  object ControlPlaneRecord extends ToZ3[Sort] {
    override def toZ3: Sort = Z3.CprSort

    val declaration: ToZ3[Decl] = new ToZ3[Decl] {
      override def toZ3: Decl = CreateCprSort
    }
  }

  case class DataFwd(r1: VertexId, r2: VertexId, acl: AccessControlList) extends ToZ3[Sym] {
    //assert(acl.r == r1)

    override def toZ3: Sym = this.toString

    override def toString: CprId = s"datafwd_${r1}_$r2"

    val declaration: ToZ3[Prog[Decl]] = new ToZ3[Prog[Decl]] {
      override def toZ3: Prog[Decl] = Seq(
        CreateSym(DataFwd.this.toString, BoolSort),
        Assert(DataFwd.this.toZ3 =? ControlFwd(r1, r2).toZ3 && acl.toZ3)
      )
    }
  }

  case class Failed(r1: VertexId, r2: VertexId) extends ToZ3[Sym] {
    override def toZ3: Sym = this.toString

    override def toString: CprId = super.toString

    val declaration: ToZ3[CreateSym] = new ToZ3[CreateSym] {
      override def toZ3: CreateSym = CreateSym(Failed.this.toString, BoolSort)
    }
  }

  case class Filter(r1: VertexId, r2: VertexId, toZ3: Expr) extends ToZ3[Expr]

  case class Graph(vs: Set[Vertex],
                   es: Set[(VertexId, VertexId)],
                   acls: Map[VertexId, AccessControlList],
                   importFilters: Set[Filter],
                   exportFilters: Set[Filter],
                   edge2Front: Map[(VertexId, VertexId), CprId],
                   edge2Back: Map[(VertexId, VertexId), CprId]) {

    val declaration: ToZ3[Prog[Decl]] = new ToZ3[Prog[Decl]] {
      def toZ3: Prog[Decl] =
        vs.map(_.declaration.toZ3).toSeq ++ // Vertices
          importFilters.map(filter => Assert(filter.toZ3)) ++ // Import filters
          exportFilters.map(filter => Assert(filter.toZ3)) ++ // Export filters
          bests.flatMap(_.declaration(bgpRouterNames.toSeq).toZ3) ++ // Bests
          controlFwds.flatMap { case (cfwd, cprId) => cfwd.declaration(cprId).toZ3 } ++ // Controlfwds
          faileds.map(_.declaration.toZ3) // Faileds
    }

    // We only check best for BGP
    val bgpRouters: Set[Vertex] = vs.filter { case Router(_, _, BGP) => true }
    val bgpRouterNames: Set[String] = bgpRouters.map(_.toString)
    val bests: Set[Best] = bgpRouters.map(r => Best(r.name, BGP))
    val controlFwds: Set[(ControlFwd, CprId)] = for { // TODO: Test controlfwd emission
      (r1, r2) <- es
      cprId <- edge2Back.get((r1, r2))
                                                      } yield (ControlFwd(r1, r2), cprId)
    val faileds: Set[Failed] = es.map { case (r1, r2) => Failed(r1, r2) }


    trait Property

    case class CanReachVertex(v: VertexId) extends Property with ToZ3[Prog[Decl]] {
      override def toZ3: Prog[Decl] = ???
    }
    
    case class FaultTolerance(k: Int) extends Property with ToZ3[Stmt] {
      override def toZ3: Stmt = Assert(Add(faileds.map(_.toZ3).toSeq) <= Num(k))
    }

    case class NeighborPreference(v: VertexId, vs: Seq[VertexId]) extends Property with ToZ3[Prog[Stmt]] {
      override def toZ3: Prog[Stmt] = {
        vs.foreach { i => assert(es.contains((i, v)), "router v doesn't contain neighbor") }
        val valids = vs.map { i =>
          val cprId = edge2Front.get((i, v)).get
          CprProj(cprId, Valid)
        }
        val ctrlfwds = vs.map { i => ControlFwd(v, i).toZ3 }
        var constraints = new ListBuffer[Stmt]
        constraints += Assert(valids(0) ==> ctrlfwds(0))
        var lhs = new ListBuffer[Expr]
        var last = valids(0)

        for (i <- (1 until vs.size)) {
          lhs += Not(last)
          constraints += Assert(And(lhs.toSeq :+ valids(i)) ==> ctrlfwds(i))
          last = valids(i)
        }
        constraints.toSeq
      }
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

  type LocalPref = Int
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
  object Packet {
    val srcIp: ToZ3[Sym] = new ToZ3[Sym] { override def toZ3: Sym = "srcIp" }
    val dstIp: ToZ3[Sym] = new ToZ3[Sym] { override def toZ3: Sym = "dstIp" }
    val srcPort: ToZ3[Sym] = new ToZ3[Sym] { override def toZ3: Sym = "srcPort" }
    val dstPort: ToZ3[Sym] = new ToZ3[Sym] { override def toZ3: Sym = "dstPort" }
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

  // Temporary main for testing.
  def main(args: Array[String]): Unit = ??? /*{
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
      val inFilter = Filter(CprProj(Sym(rec1), Z3.Valid) =? Bool(true))
      val exFilter = Filter(CprProj(Sym(rec2), Z3.Valid) =? Bool(true))

      val vertices = Set(r1, r2)
      val edges = Set(edge)
      val edge2Import = Map(edge -> inFilter)
      val edge2Export = Map(edge -> exFilter)
      val edge2Front = Map(edge -> rec1)
      val edge2Back = Map(edge -> rec2)

      val graph = Graph(vertices, edges, Map(), edge2Import, edge2Export, edge2Front, edge2Back)

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
  }*/
}
