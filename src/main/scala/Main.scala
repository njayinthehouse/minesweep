import frontEnd.Property
import Network.{Graph, Packet}
import smt.Z3.{Sat, Model, Stmts}

object Main {
  def main(graph: Graph, packet: Packet, properties: Seq[Property]): Unit = {
    print(packet.toZ3 ++ graph.toZ3 ++ properties.map(_.toZ3).fold(Stmts(Seq()))(_++_) ++ Sat ++ Model)
  }
}