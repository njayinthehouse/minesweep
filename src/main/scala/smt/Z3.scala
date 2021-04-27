package smt

// Thin wrapper on Z3 smt-lib
object Z3 {
  trait T

  case class BitVecSort(length: Int) extends Sort
  case object IntS extends Sort

  case class Sym(x: String) extends Expr
  case class Num(n: Int) extends Expr
  case class Hex(s: String) extends Expr

  case class CreateSym(x: String, sort: Sort) extends Stmt
  case class CreateEqual(e: Expr, u: Expr) extends Stmt
  case object Sat extends Stmt
  case object Model extends Stmt
  case class Stmts(ss: collection.Seq[Stmt]) extends Stmt

  abstract class Sort extends T

  abstract class Expr extends T {
    override def toString: String = this match {
      case Sym(x) => x
      case Num(n) => n toString
      case Hex(s) => s
    }
  }

  abstract class Stmt extends T {
    override def toString: String = this match {
      case CreateSym(x, sort) => s"(declare-const $x $sort)"
      case CreateEqual(e, u) => s"(assert (= $e $u))"
      case Sat => "(check-sat)"
      case Model => "(get-model)"
      case Stmts(ss) => ss.map(_.toString).foldLeft("\n")(_ ++ _)
    }

    def toSeq: Seq[Stmt] = this match {
      case Stmts(ss) => ss
      case s => Seq(s)
    }

    def ++(that: Stmt): Stmts = Stmts(this.toSeq ++ that.toSeq)
    def ++(that: Option[Stmt]): Stmt = if (that isEmpty) this else this ++ that.get
  }
}

trait ToZ3[T <: Z3.T] {
  def toZ3: T
}
