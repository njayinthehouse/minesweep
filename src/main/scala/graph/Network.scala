package graph

import smt.Z3
import smt.Z3._

import scala.language.{implicitConversions, postfixOps}

object Network {

  case class Graph(vs: Set[Vertex], es: Set[Edge])

  trait Address

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

  case class Edge(v: Vertex, w: Vertex, back: ControlPlaneRec, front: ControlPlaneRec)

  // TODO: Filters
  trait ImportFilter

  case class Ip(ip: Int) {
    def toHexString: String = "#x" ++ ip.toHexString.reverse.padTo(8, '0').reverse
  }
  object Ip {
    val length: Int = 32
    implicit def of(x: Int): Ip = Ip(x)
  }

  trait IpPrefix

  type Metric = Int
  type MED = Int

  case class Packet(srcIp: Option[Ip], srcPort: Option[Port], dstIp: Option[Ip], dstPort: Option[Port]) {
    def toZ3: Z3.T = {
      createSym("srcIp", bitVecSort(Ip.length)) ++
      createSym("srcPort", intSort) ++
      createSym("dstIp", bitVecSort(Ip.length)) ++
      createSym("dstPort", intSort) ++
      srcIp.map(ip => createEqual("srcIp", ip toHexString)).getOrElse("") ++
      srcPort.map(port => createEqual("srcPort", port.port toString)).getOrElse("") ++
      dstIp.map(ip => createEqual("dstIp", ip toHexString)).getOrElse("") ++
      dstPort.map(port => createEqual("dstPort", port.port toString)).getOrElse("")
    }
  }

  // Temporary main for testing.
  def main(args: Array[String]): Unit = print(Packet(Some(1), Some(2), Some(3), Some(4)).toZ3)

  case class Port(port: Int)
  object Port {
    implicit def of(x: Int): Port = Port(x)
  }

  trait Protocol

  case object OSPF extends Protocol

  case object BGP extends Protocol

  case object CON extends Protocol

  trait Vertex {
    val name: String
  }

  type Vrf = Map[Edge, ControlPlaneRec]

  // Routers are subrouters running a protocol
  case class Router(name: String, ip: Ip, protocol: Protocol) extends Vertex

  case class Neighbor(name: String, ip: Ip) extends Vertex

  // Chose not to put a graph in subnet, since we're abstracting over that
  case class Subnet(name: String, prefix: IpPrefix) extends Vertex

}
