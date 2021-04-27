package smt

// Thin wrapper on Z3 smt-lib
object Z3 {
  type Sym = String

  abstract class Sort
  case class BitVec(length: Int) extends Sort
  case object Int extends Sort

  case class CreateSym(x: Sym, sort: Sort) extends Stmt
  case class CreateEqual(e: Stmt, u: Stmt) extends Stmt
  case object Sat extends Stmt
  case object Model extends Stmt
  case class Seq(stmts: collection.Seq[Stmt]) extends Stmt

  abstract class Stmt {
    override def toString: String = this match {
      case CreateSym(x, sort) => s"(declare-const $x $sort)"
      case CreateEqual(e, u) => s"(assert (= $e $u))"
      case Sat => "(check-sat)"
      case Model => "(get-model)"
      case Seq(ss) => ss.map(_.toString).foldLeft("")(_ ++ _)
    }
  }
}

trait ToZ3 {
  def toZ3: Z3.Stmt
}
