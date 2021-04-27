import Network.Ip
import smt.{ToZ3, Z3}

// MineSweeper-lang
abstract class MS extends ToZ3[Z3.Expr] {

  def freeVars: Set[Var] = this match {
    case Var(cpr, x) => Set(Var(cpr, x))
    case And(cs) => cs.flatMap(_.freeVars) toSet
    case Or(ds) => ds.flatMap(_.freeVars) toSet
    case Not(e) => e.freeVars
    case If(c, t, e) => c.freeVars ++ t.freeVars ++ e.freeVars
    case FBM(x, y, len) => x.freeVars ++ y.freeVars ++ len.freeVars
    case _ => Set()
  }

  def toZ3: Z3.Expr = ???

}
case class Cpr(name: String) extends MS // Control plane record
case class Var(cpr: Cpr, field: String) extends MS
case class Num(n: Int) extends MS
case class Bool(b: Boolean) extends MS
case class IP(ip: Ip) extends MS
case class And(conjuncts: Seq[MS]) extends MS
case class Or(disjuncts: Seq[MS]) extends MS
case class Not(e: MS) extends MS
case class If(c: MS, t: MS, f: MS) extends MS

case class FBM(prefix: Var, ip: Var, length: Num) extends MS
case class Eq(lhs: MS, rhs: MS) extends MS
case class DataFwd(r1: String, r2: String) extends MS
case class ControlFwd(r1: String, r2: String) extends MS
case class Preferred(r1: Cpr, r2: Cpr) extends MS
