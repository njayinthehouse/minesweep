import Network.{createMask, VertexId}

package smt {

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

    trait Id extends Expr

    case class Sym(name: String) extends Id

    case class CprProj(x: String, proj: ControlPlaneField) extends Id

    case class Num(n: Int) extends Expr

    case class Bool(b: Boolean) extends Expr

    case class Hex(s: String) extends Expr

    case class Add(es: Seq[Expr]) extends Expr

    case class And(es: Seq[Expr]) extends Expr

    case class Or(es: Seq[Expr]) extends Expr

    case class Lt(e: Expr, u: Expr) extends Expr

    case class Le(e: Expr, u: Expr) extends Expr

    case class Eq(e: Expr, u: Expr) extends Expr

    case class Not(e: Expr) extends Expr

    case class BvXor(e: Expr, u: Expr) extends Expr

    case class Implies(e: Expr, u: Expr) extends Expr

    case class If(c: Expr, t: Expr, e: Expr) extends Expr


    case class CreateSym(x: String, sort: Sort) extends Decl

    case object CreateCprSort extends Decl

    case class Assert(e: Expr) extends Decl

    case class Echo(s: String) extends Decl

    case object Sat extends Stmt

    case object Model extends Stmt

    abstract class Sort extends T {
      override def toString: String = this match {
        case IntSort => "Int"
        case BoolSort => "Bool"
        case CprSort => CPR_SORT_NAME
        case BitVecSort(l) => s"(_ BitVec $l)"
      }
    }

    abstract class Expr extends T {
      override def toString: String = this match {
        case Sym(x) => x
        case Num(n) => n.toString
        case Bool(b) => b.toString
        case Hex(s) => s
        case Add(es) => s"(+${es.map(_.toString).foldLeft("")((x, y) => s"$x $y")})"
        case And(es) => if (es.isEmpty) "true" else s"(and${es.map(_.toString).foldLeft("")((x, y) => s"$x $y")})"
        case Or(es) => if (es.isEmpty) "true" else s"(or${es.map(_.toString).foldLeft("")((x, y) => s"$x $y")})"
        case Lt(e, u) => s"(< $e $u)"
        case Le(e, u) => s"(<= $e $u)"
        case Eq(e, u) => s"(= $e $u)"
        case Not(e) => s"(not $e)"
        case BvXor(e, u) => s"(bvxor $e $u)"
        case Implies(e, u) => s"(=> $e $u)"
        case If(c, t, e) => s"(ite $c $t $e)"
        case CprProj(x, proj) => s"($proj $x)"
      }

      def freeSyms: Set[String] = this match {
        case Sym(x) => Set(x)
        case And(es) => es.flatMap(_.freeSyms).toSet
        case Or(es) => es.flatMap(_.freeSyms).toSet
        case Lt(e, u) => e.freeSyms ++ u.freeSyms
        case Le(e, u) => e.freeSyms ++ u.freeSyms
        case Eq(e, u) => e.freeSyms ++ u.freeSyms
        case Not(e) => e.freeSyms
        case BvXor(e, u) => e.freeSyms ++ u.freeSyms
        case Implies(e, u) => e.freeSyms ++ u.freeSyms
        case If(c, t, e) => c.freeSyms ++ t.freeSyms ++ e.freeSyms
        case CprProj(x, _) => Set(x)
        case _ => Set()
      }

      def &&(that: Expr): And = And(Seq(this, that))

      def ||(that: Expr): Or = Or(Seq(this, that))

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
          s"(declare-datatypes () (($CPR_SORT_NAME (mk-cpr " +
            s"(prefix ${BitVecSort(32)}) " +
            s"(length $IntSort) " +
            s"(ad $IntSort) " +
            s"(lp $IntSort) " +
            s"(metric $IntSort) " +
            s"(med $IntSort) " +
            s"(rid $IntSort) " +
            s"(bgpInternal $BoolSort) " +
            s"(valid $BoolSort)))))"
        }
        case Assert(e) => s"(assert $e)"
        case Sat => "(check-sat)"
        case Model => "(get-model)"
        case Echo(s) => "(echo " + "\"" + s + "\")"
      }
    }

    case class Prog[S <: Stmt](ss: Seq[S]) extends T {
      override def toString: String = ss.map(_.toString).foldLeft("")((x, y) => s"$x\n$y")

      def withDCE: Prog[S] = {
        var syms: Set[Sym] = Set()
        var r: Seq[S] = Seq()

        for (s <- ss.reverse)
          s match {
            case CreateSym(x, _) => if (syms.contains(Sym(x))) r ++= Seq(s)
            case Assert(e) => {
              syms ++= e.freeSyms.map(Sym(_))
              r ++= Seq(s)
            }
            case _ => r ++= Seq(s)
          }

        Prog(r.reverse)
      }
    }

    // Additional API
    def Preferred(r1: String, r2: String): Or =
      (( CprProj(r2, Ad) <= CprProj(r1, Ad))
      || (CprProj(r2, Ad) =? CprProj(r1, Ad) && CprProj(r2, Lp) <= CprProj(r1, Lp))
      || (CprProj(r2, Ad) =? CprProj(r1, Ad) && CprProj(r2, Lp) =? CprProj(r1, Lp)  && CprProj(r2, Metric) <= CprProj(r1, Metric)))


    def FBM(prefix: Expr, ip: Expr, length: Int): Expr =
      BvXor(prefix, Hex(createMask(length))) =? BvXor(ip, Hex(createMask(length)))

    object Prog {
      implicit def of[S <: Stmt](ss: Seq[S]): Prog[S] = Prog(ss)

      implicit def to[S <: Stmt](p: Prog[S]): Seq[S] = p.ss
    }

    object Sym {
      implicit def of(s: String): Sym = Sym(s)

      implicit def to(s: Sym): String = s.name
    }

    object Num {
      implicit def of(n: Int): Num = Num(n)

      implicit def to(n: Num): Int = n.n
    }

    object Bool {
      implicit def of(b: Boolean): Bool = Bool(b)

      implicit def to(b: Bool): Boolean = b.b
    }
  }

  trait ToZ3[T <: Z3.T] {
    def toZ3: T
  }

}