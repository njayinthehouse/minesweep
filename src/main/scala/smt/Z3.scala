package smt

// Thin wrapper on Z3 smt-lib
object Z3 {
  trait T

  case class BitVecSort(length: Int) extends Sort
  case object IntSort extends Sort

  case class Sym(x: String) extends Expr
  case class Num(n: Int) extends Expr
  case class Hex(s: String) extends Expr
  case class And(e: Expr, es: Seq[Expr]) extends Expr
  case class Or(e: Expr, es: Seq[Expr]) extends Expr
  case class Lt(e: Expr, u: Expr) extends Expr
  case class Eq(e: Expr, u: Expr) extends Expr

  case class CreateSym(x: String, sort: Sort) extends Decl
  case class CreateRecord(name: String, fields: Seq[(String, Sort)]) extends Decl
  case class Assert(e: Expr) extends Decl
  case object Sat extends Stmt
  case object Model extends Stmt

  abstract class Sort extends T

  abstract class Expr extends T {
    override def toString: String = this match {
      case Sym(x) => x
      case Num(n) => n toString
      case Hex(s) => s
      case And(e, es) => s"(and $e ${es.flatMap(_.toString)})"
      case Or(e, es) => s"(or $e ${es.flatMap(_.toString)}"
    }

    def <(that: Expr): Lt = Lt(this, that)
    def &&(that: Expr): And = And(this, Seq(that))
    def ||(that: Expr): Or = Or(this, Seq(that))
    def =?(that: Expr): Eq = Eq(this, that)
  }

  abstract class Decl extends Stmt

  abstract class Stmt extends T {
    override def toString: String = this match {
      case CreateSym(x, sort) => s"(declare-const $x $sort)"
      case CreateRecord(name, fields) => {
        s"(declare-datatypes () ($name (mk-$name " +
          s"${fields.flatMap{ case (x, s) => s"(x s)" }})))"
      }
      case Assert(e) => s"(assert $e)"
      case Sat => "(check-sat)"
      case Model => "(get-model)"
    }
  }

  case class Prog[S <: Stmt](ss: Seq[S]) extends T
  object Prog {
    implicit def of[S <: Stmt](ss: Seq[S]): Prog[S] = Prog(ss)
  }
}

trait ToZ3[T <: Z3.T] {
  def toZ3: T
}

