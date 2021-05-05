import org.scalatest.FunSuite

import java.io._ 
import Network._
import smt.Z3
import smt.Z3._

// Testing framework
// run `sbt test` in the root directory to test all testcases
// run `sbt testOnly *MinesweepTest -- -t TEST_NAME` to run test case TEST_NAME
// test case golden outputs are under src/out
// set overwriteCheckFiles to false to make sure the output file doesn't change
// set overwriteCheckFiles to true to update the .check file
// if the output doesn't match the golden .check file, it will cause the test case
// to fail and the actual output is names as TEST_NAME.actual.z3

class MinesweepTest extends FunSuite {
  val overwriteCheckFiles = false

  def readFile(name: String): String = {
    try {
      val buf = new Array[Byte](new File(name).length().toInt)
      val fis = new FileInputStream(name)
      fis.read(buf)
      fis.close()
      new String(buf)
    } catch {
      case e: IOException => ""
    }
  }

  def deleteFile(name: String): Unit = {
    val file = new File(name)
    if (file.exists) {
      file.delete()
    }
  }

  def writeTo(code: String, name: String) {
    val file = new File(name)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(code)
    bw.close()
  }

  def check(test: Seq[Z3.Stmt], name: String) {
    val fileName = "src/out/" + name + ".check.z3"
    val actualName = "src/out/" + name + ".actual.z3"
    val code = test.map(_.toString).reduce((x, y) => x + "\n" + y) + "\n"
    val expected = readFile(fileName)
    deleteFile(actualName)
    if (overwriteCheckFiles) {
      writeTo(code, name)
    } else if (code != expected) {
      writeTo(code, actualName)
      assert(false, name)
    }
  }

  test("fbm_test") {
    val FBM_test = Seq(
      CreateCprSort,
      CreateSym("c", Z3.CprSort),
      Assert(FBM(CprProj("c", Z3.Prefix), CprProj("c", Z3.Prefix), 5)),
      Z3.Sat,
      Z3.Model
    )
    check(FBM_test, "fbm")
  }

  test("reachability") {
    val r1: Vertex = Router(1, Ip(1111), BGP)
    val r2: Vertex = Router(2, Ip(2222), BGP)
    val r3: Vertex = Router(3, Ip(3333), BGP)
    val r4: Vertex = Router(4, Ip(4444), BGP)
    val edge1 = (1, 2)
    val edge2 = (2, 3)

    val edge1_front = "edge1_front"
    val edge1_back = "edge1_back"
    val edge2_front = "edge2_front"
    val edge2_back = "edge2_back"

    val edgeToFront = Map(edge1 -> edge1_front, edge2 -> edge2_front)
    val edgeToBack = Map(edge1 -> edge1_back, edge2 -> edge2_back)

    val vertices = Set(r1, r2, r3, r4)
    val edges = Set(edge1, edge2)

    val graph = Graph(vertices, edges, Map(), Set(), Set(), edgeToFront, edgeToBack)

    val REACHABILITY_test = Seq(
      CreateCprSort,
      CreateSym(edge1_front, Z3.CprSort),
      CreateSym(edge1_back, Z3.CprSort),
      CreateSym(edge2_front, Z3.CprSort),
      CreateSym(edge2_back, Z3.CprSort)
    ) ++
      graph.declaration.toZ3.ss ++
        graph.CanReachFrom(1)(4).toZ3.ss ++
      Seq(
        Z3.Sat,
        Z3.Model
      )

    check(REACHABILITY_test, "reachability")
  }

  test("isolation") {
    val r1: Vertex = Router(1, Ip(1111), BGP)
    val r2: Vertex = Router(2, Ip(2222), BGP)
    val r3: Vertex = Router(3, Ip(3333), BGP)
    val r4: Vertex = Router(4, Ip(4444), BGP)
    val edge1 = (1, 2)
    val edge2 = (2, 3)

    val edge1_front = "edge1_front"
    val edge1_back = "edge1_back"
    val edge2_front = "edge2_front"
    val edge2_back = "edge2_back"

    val edgeToFront = Map(edge1 -> edge1_front, edge2 -> edge2_front)
    val edgeToBack = Map(edge1 -> edge1_back, edge2 -> edge2_back)

    val vertices = Set(r1, r2, r3, r4)
    val edges = Set(edge1, edge2)

    val graph = Graph(vertices, edges, Map(), Set(), Set(), edgeToFront, edgeToBack)

    val ISOLATION_test = Seq(
      CreateCprSort,
      CreateSym(edge1_front, Z3.CprSort),
      CreateSym(edge1_back, Z3.CprSort),
      CreateSym(edge2_front, Z3.CprSort),
      CreateSym(edge2_back, Z3.CprSort)
    ) ++
      graph.declaration.toZ3.ss ++
      graph.IsolatedFrom(1)(4).toZ3.ss ++
      Seq(
        Z3.Sat,
        Z3.Model
      )

    check(ISOLATION_test, "isolation")
  }

  test("fault-tolerance") {???}

  test("graph_test") {
    val r1: Vertex = Router(1, Ip(12345), BGP)
    val r2: Vertex = Router(2, Ip(54321), BGP)
    val edge = (1, 2)
    val front = "front_rec"
    val back = "back_rec"

    val acl1 = AccessControlList(Seq())
    val acl2 = AccessControlList(Seq())

    val vertices = Set(r1, r2)
    val edges = Set(edge)
    val edge2Front = Map((edge -> front))
    val edge2Back = Map((edge -> back))
    val acl = Map((1 -> acl1), (2 -> acl2))

    val graph = Graph(vertices, edges, acl, Set(), Set(), edge2Front, edge2Back)

    val GRAPH_test = Seq(
      CreateCprSort,
      CreateSym(front, Z3.CprSort),
      CreateSym(back, Z3.CprSort)
    ) ++
      graph.declaration.toZ3.ss ++
      Seq(
        Z3.Sat,
        Z3.Model
      )
    check(GRAPH_test, "graph")
  }
}