import frontEnd.Property
import Network.{Graph, Packet}
import smt.Z3.{sat, model}

object Main {
  def main(graph: Graph, packet: Packet, properties: Seq[Property]): Unit = {
    print(packet.toZ3 ++ graph.toZ3 ++ properties.flatMap(_.toZ3) ++ sat ++ model)
  }
}