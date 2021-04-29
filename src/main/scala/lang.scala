import Network.{Ip, createMask}
import smt.Z3.{BitVecSort, ControlPlaneField, CprProj, Sym}
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
    case Var(name) => Z3.Sym(name)
    case Proj(cpr, field) => Z3.CprProj(cpr.name, field)
    case Num(n) => Z3.Num(n)
    case Bool(b) => Z3.Bool(b)
    case IP(ip) => ip.toZ3
    case And(c :: cs) => Z3.And(c.toZ3, cs.map(_.toZ3))
    case Or(c :: cs) => Z3.Or(c.toZ3, cs.map(_.toZ3))
    case Not(e) => Z3.Not(e.toZ3)
    case If(c, t, f) => Z3.If(c.toZ3, t.toZ3, f.toZ3)
    case Implies(e, u) => Z3.Implies(e.toZ3, u.toZ3)
    case Eq(lhs, rhs) => Z3.Eq(lhs.toZ3, rhs.toZ3)
    case Lt(e, u) => Z3.Lt(e.toZ3, u.toZ3)
    case Le(e, u) => Z3.Le(e.toZ3, u.toZ3)

    case DataFwd(r1, r2) => Z3.Sym(s"DataFwd_${r1}_$r2")
    case ControlFwd(r1, r2) => Z3.Sym(s"ControlFwd_${r1}_$r2")
    case Preferred(r1, r2) => ((CprProj(r2.name, Z3.Ad) <= CprProj(r1.name, Z3.Ad))
      || (CprProj(r2.name, Z3.Ad) =? CprProj(r1.name, Z3.Ad) && CprProj(r2.name, Z3.Lp) <= CprProj(r1.name, Z3.Lp))
      || (CprProj(r2.name, Z3.Ad) =? CprProj(r1.name, Z3.Ad) && CprProj(r2.name, Z3.Lp) =? CprProj(r1.name, Z3.Lp)
      && CprProj(r2.name, Z3.Metric) <= CprProj(r1.name, Z3.Metric)))
    case FBM(prefix, ip, length) => Z3.Eq(Z3.BvXor(prefix.toZ3, Z3.Hex(createMask(length.n))),
                                          Z3.BvXor(ip.toZ3, Z3.Hex(createMask(length.n))))

  }

}
case class Var(name: String) extends MS // Control plane record
case class Proj(cpr: Var, field: ControlPlaneField) extends MS
case class Num(n: Int) extends MS
case class Bool(b: Boolean) extends MS
case class IP(ip: Ip) extends MS
case class And(conjuncts: Seq[MS]) extends MS
case class Or(disjuncts: Seq[MS]) extends MS
case class Not(e: MS) extends MS
case class If(c: MS, t: MS, f: MS) extends MS
case class Implies(e: MS, u: MS) extends MS
case class Eq(lhs: MS, rhs: MS) extends MS
case class Lt(e: MS, u: MS) extends MS
case class Le(e: MS, u: MS) extends MS

case class DataFwd(r1: Int, r2: Int) extends MS
case class ControlFwd(r1: Int, r2: Int) extends MS
case class Preferred(r1: Var, r2: Var) extends MS
case class FBM(prefix: Proj, ip: Proj, length: Num) extends MS