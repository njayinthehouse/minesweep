package graph

import scala.collection.mutable
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


  case class Ip(ip: Int) {
    def toHexString: String = "#x" ++ ip.toHexString.reverse.padTo(8, '0').reverse
  }
  object Ip {
    val length: Int = 32
    implicit def of(x: Int): Ip = Ip(x)
  }

  trait IpPrefix {
    val prefix: Int
  }

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


  // Vertex is a device (node) in the network graph
  trait Vertex
  // Routers are subrouters running a protocol
  case class Router(name: String, ip: Ip, protocol: Protocol) extends Vertex
  case class Neighbor(name: String, ip: Ip) extends Vertex
  // Chose not to put a graph in subnet, since we're abstracting over that
  case class Subnet(name: String, prefix: IpPrefix) extends Vertex

  type Vrf = Map[Edge, ControlPlaneRec]



  // Command is the user API
  abstract class Command
  case class AddEdge(src: Vertex, dst: Vertex) extends Command
  case class Deny(vertex: Vertex, prefix: IpPrefix, mask: Int, range: (Int, Int)) extends Command
  case class SetLocalPreference(vertex: Vertex, pref: Int) extends Command

  object Reader {
    var cmds = mutable.ListBuffer[Command]()
    var tokens = mutable.Queue[String]()

    def getProtocol: Protocol = tokens.dequeue() match {
      case "BGP" => BGP
      case "OSPF" => OSPF
      case "CON" => CON
    }

    def getVertex: Vertex = tokens.dequeue() match {
      case "Router" => val name = tokens.dequeue();
        val ip = Ip(tokens.dequeue().toInt); val protocol = getProtocol; Router(name, ip, protocol)
      case "Neighbor" => val name = tokens.dequeue();
        val ip = Ip(tokens.dequeue().toInt); Neighbor(name, ip)
      case "Subnet" => val name = tokens.dequeue();
        val prefix = IpPrefix(tokens.dequeue().toInt); Subnet(name, prefix)
    }

    def getCmd: Command = tokens.dequeue() match {
      case "AddEdge" => val src = getVertex; val dst = getVertex; AddEdge(src, dst)
    }

    // convert a list of String to a list of Command
    def read(file: List[String]): List[Command] = file map { l =>
      tokens.clear; tokens ++= l.split(" +"); getCmd
    }
  }

  object Parser {
    var vertices = mutable.Set[Vertex]()
    var edges = mutable.Set[Edge]()

    // dummy values for now
    val back = ControlPlaneRec(IpPrefix(0))
    val front = ControlPlaneRec(IpPrefix(0))

    def parseEach(xs: List[Command]): Unit = xs.foreach {
      case AddEdge(src, dst) =>
        // add edge src->dst and dst->src into graph
        vertices += src; vertices += dst; edges += Edge(src, dst, back, front); edges += Edge(dst, src, back, front)
      case _ => System.out.println("not supported yet")
    }

    def parse(xs: List[Command]): Graph = {
      parseEach(xs)
      Graph(vertices.toSet, edges.toSet)
    }
  }
}
