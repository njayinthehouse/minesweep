import Network.Ip
import smt.Z3.{CprProj, Sym}
import smt.{ToZ3, Z3}

// MineSweeper-lang
abstract class MS extends ToZ3[Z3.Expr] {

  def freeVars: Set[Proj] = this match {
    case Proj(cpr, x) => Set(Proj(cpr, x))
    case And(cs) => cs.flatMap(_.freeVars).toSet
    case Or(ds) => ds.flatMap(_.freeVars).toSet
    case Not(e) => e.freeVars
    case If(c, t, e) => c.freeVars ++ t.freeVars ++ e.freeVars
    case FBM(x, y, len) => x.freeVars ++ y.freeVars ++ len.freeVars
    case _ => Set()
  }

  def toZ3: Z3.Expr = this match {
    case Preferred(r1, r2) => ((CprProj(r2.name, Z3.Ad) <= CprProj(r1.name, Z3.Ad))
      || (CprProj(r2.name, Z3.Ad) =? CprProj(r1.name, Z3.Ad) && CprProj(r2.name, Z3.Lp) <= CprProj(r1.name, Z3.Lp))
      || (CprProj(r2.name, Z3.Ad) =? CprProj(r1.name, Z3.Ad) && CprProj(r2.name, Z3.Lp) =? CprProj(r1.name, Z3.Lp)
          && CprProj(r2.name, Z3.Metric) <= CprProj(r1.name, Z3.Metric)))
  }

}
case class Var(name: String) extends MS // Control plane record
case class Proj(cpr: Var, field: String) extends MS
case class Num(n: Int) extends MS
case class Bool(b: Boolean) extends MS
case class IP(ip: Ip) extends MS
case class And(conjuncts: Seq[MS]) extends MS
case class Or(disjuncts: Seq[MS]) extends MS
case class Not(e: MS) extends MS
case class If(c: MS, t: MS, f: MS) extends MS
case class Implies(e: MS, u: MS) extends MS

case class FBM(prefix: Proj, ip: Proj, length: Num) extends MS
case class Eq(lhs: MS, rhs: MS) extends MS
case class DataFwd(r1: String, r2: String) extends MS
case class ControlFwd(r1: String, r2: String) extends MS
case class Preferred(r1: Var, r2: Var) extends MS