import Network.Graph.Router
import smt.Z3.Sym.of
import smt.{ToZ3, Z3}
import smt.Z3._

object Network {

  type VertexId = Int

  case class Best(routerName: VertexId, protocol: Protocol) extends ToZ3[Sym] {
    override def toZ3: Sym = s"r${routerName}_best_$protocol"

    object Declaration extends ToZ3[Decl] {
      def toZ3: Decl = ???
    }
  }

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

      private def best(vName: Int, protocol: Protocol): Prog[Decl] =
        if (protocol == BGP) Seq (
          CreateSym(s"${vName}_best_$protocol", Z3.CprSort),
          Assert (Or (for {
            ((uName, _), cprName) <- edge2Front.toSeq
            if vName == uName
          } yield Best(vName, protocol).toZ3 =? cprName)),
          Assert (And (for {
            ((uName, _), cprName) <- edge2Front.toSeq
            if vName == uName
          } yield Preferred(Best(vName, protocol).toZ3, cprName)))
        ) else Seq()

    override def toZ3: Prog[Decl] =
      (vs.map(_.toZ3)).toSeq ++
        (vs.flatMap{ case r: Router => best(r.name, r.protocol) })
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

  def ControlFwd(r1: VertexId, r2: VertexId): Sym = Sym(s"ControlFwd_${r1}_$r2")

  object ControlPlaneRecord extends ToZ3[Sort] {
    override def toZ3: Sort = Z3.CprSort

    object Declaration extends ToZ3[Decl] {
      override def toZ3: Decl = CreateCprSort
    }
  }

  def DataFwd(r1: VertexId, r2: VertexId): Sym = Sym(s"DataFwd_${r1}_$r2")

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


  case class Port(port: Int) extends ToZ3[Z3.Num] {
    override def toZ3: Z3.Num = Z3.Num(port)
  }

  trait Protocol
  case object OSPF extends Protocol
  case object BGP extends Protocol
  case object CON extends Protocol
}
