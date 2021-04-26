package smt

// Thin wrapper on Z3 smt-lib
object Z3 {
  type T = String

  def createSym(x: T, sort: T): T = s"(declare-const $x $sort)\n"
  def createEqual(x: T, y: T): T = s"(assert (= $x $y))\n"
  def bitVecSort(length: Int): T = s"(_ BitVec $length)"
  val intSort: T = "Int"
}

trait ToZ3[T] {
  def toZ3(t: T): Z3.T
}
