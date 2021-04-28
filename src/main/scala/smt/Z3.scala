package smt

// Thin wrapper on Z3 smt-lib
object Z3 {
  val CPR_SORT_NAME = "ControlPlaneRecord"


  case object Prefix extends ControlPlaneField
  case object Length extends ControlPlaneField
  case object Ad extends ControlPlaneField
  case object Lp extends ControlPlaneField
  case object Metric extends ControlPlaneField
  case object Med extends ControlPlaneField
  case object Rid extends ControlPlaneField
  case object BgpInternal extends ControlPlaneField
  case object Valid extends ControlPlaneField

  abstract class ControlPlaneField {
    override def toString: String = this match {
      case Prefix => "prefix"
      case Length => "length"
      case Ad => "ad"
      case Lp => "lp"
      case Metric => "metric"
      case Med => "med"
      case Rid => "rid"
      case BgpInternal => "bgpInternal"
      case Valid => "valid"
    }
  }

  trait T

  case class BitVecSort(length: Int) extends Sort
  case object IntSort extends Sort
  case object BoolSort extends Sort
  case object CprSort extends Sort

  case class Sym(x: String) extends Expr
  case class Num(n: Int) extends Expr
  case class Hex(s: String) extends Expr
  case class And(e: Expr, es: Seq[Expr]) extends Expr
  case class Or(e: Expr, es: Seq[Expr]) extends Expr
  case class Lt(e: Expr, u: Expr) extends Expr
  case class Le(e: Expr, u: Expr) extends Expr
  case class Eq(e: Expr, u: Expr) extends Expr
  case class Not(e: Expr) extends Expr
  case class Implies(e: Expr, u: Expr) extends Expr
  case class If(c: Expr, t: Expr, e: Expr) extends Expr
  case class CprProj(x: String, proj: ControlPlaneField) extends Expr

  case class CreateSym(x: String, sort: Sort) extends Decl
  case object CreateCprSort extends Decl
  case class Assert(e: Expr) extends Decl
  case object Sat extends Stmt
  case object Model extends Stmt

  abstract class Sort extends T {
    override def toString: String = this match {
      case IntSort => "Int"
      case BoolSort => "Bool"
      case CprSort => CPR_SORT_NAME
    }
  }

  abstract class Expr extends T {
    override def toString: String = this match {
      case Sym(x) => x
      case Num(n) => n.toString
      case Hex(s) => s
      case And(e, es) => s"(and $e ${es.flatMap(_.toString)})"
      case Or(e, es) => s"(or $e ${es.flatMap(_.toString)}"
      case Lt(e, u) => s"(< $e $u)"
      case Le(e, u) => s"(<= $e $u)"
      case Eq(e, u) => s"(= $e $u)"
      case Not(e) => s"(not $e)"
      case Implies(e, u) => s"(=> $e $u)"
      case If(c, t, e) => s"(ite $c $t $e)"
      case CprProj(x, proj) => s"($proj $x)"
    }

    def &&(that: Expr): And = And(this, Seq(that))
    def ||(that: Expr): Or = Or(this, Seq(that))
    def <(that: Expr): Lt = Lt(this, that)
    def <=(that: Expr): Le = Le(this, that)
    def =?(that: Expr): Eq = Eq(this, that)
    def ==>(that: Expr): Implies = Implies(this, that)
  }

  abstract class Decl extends Stmt

  abstract class Stmt extends T {
    override def toString: String = this match {
      case CreateSym(x, sort) => s"(declare-const $x $sort)"
      case CreateCprSort => {
        s"(declare-datatypes () ($CPR_SORT_NAME (mk-cpr " +
          s"(prefix ${BitVecSort(32)}) " +
          s"(length $IntSort) " +
          s"(ad $IntSort) " +
          s"(lp $IntSort) " +
          s"(metric $IntSort) " +
          s"(med $IntSort) " +
          s"(rid $IntSort) " +
          s"(bgpInternal $BoolSort) " +
          s"(valid $BoolSort)"
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

