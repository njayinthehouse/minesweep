import Network.Ip

// MineSweeper-lang
abstract class MS extends smt.ToZ3 {

  def freeVars: Set[Var] = this match {
    case Var(cpr, x) => Set(Var(cpr, x))
    case And(cs) => cs.flatMap(_.freeVars) toSet
    case Or(ds) => ds.flatMap(_.freeVars) toSet
    case Not(e) => e.freeVars
    case If(c, t, e) => c.freeVars ++ t.freeVars ++ e.freeVars
    case FBM(x, y, len) => x.freeVars ++ y.freeVars ++ len.freeVars
    case _ => Set()
  }

  def toZ3: smt.Z3.T = ???

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