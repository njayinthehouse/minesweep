import org.scalatest.FunSuite

import java.io._ 
import Network._
import Network.Graph._
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
  
  test("graph_test") {
    val r1: Vertex = Router(1, Ip(12345), BGP)
    val r2: Vertex = Router(2, Ip(54321), BGP)
    val edge = (1, 2)
    val rec1 = "rec1"
    val rec2 = "rec2"
    val inFilter = Eq(CprProj(Sym(rec1), Z3.Valid), Bool(true))
    val exFilter = Eq(CprProj(Sym(rec2), Z3.Valid), Bool(true))

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
      graph.declaration.toZ3.ss ++
      Seq(
        Z3.Sat,
        Z3.Model
      )
    check(GRAPH_test, "graph")
  }

  test("Slicing1") {
    val vertices = for (i <- 1 to 10000) yield Router(i, Ip(i + 5), BGP)
    val edges = for (i <- 1 to 10) yield (i, i + 1)
    val rec1 = "rec1"
    val rec2 = "rec2"

    val edge2Import = Map()
    val edge2Export = Map()
    val edge2Front = Map()
    val edge2Back = Map()

    val graph = Graph(vertices, edges, edge2Import, edge2Export, edge2Front, edge2Back)

    val GRAPH_test = Seq(
      CreateCprSort,
      CreateSym("rec1", Z3.CprSort),
      CreateSym("rec2", Z3.CprSort)
    ) ++
      graph.declaration.toZ3.ss ++
      Seq(
        Z3.Sat,
        Z3.Model
      )

    check(Prog(GRAPH_test).withDCE, name = "graph_dce")
  }
}
