/*import Network.IpPrefix
import smt.{ToZ3, Z3}

object MatchAction {

  trait Match

  case class Filter(coms: Seq[FilterLang]) extends Match with ToZ3[Z3.Expr] {
    override def toZ3: Z3.Expr = Z3.And(coms.map(_.toZ3))
  }
  case class AccessControlList(acl: Seq[Acl]) extends Match

  trait FilterLang {
    def withCpr(cprSym: Z3.Sym): ToZ3[Z3.Expr] = new ToZ3[Z3.Expr] {
      override def toZ3: Z3.Expr = ???
    }
  }
  case class PrefixList(prefixes: Set[Network.IpPrefix]) extends FilterLang
  case class Ip(ip: Network.Ip) extends FilterLang
  case class Metric(metric: Network.Metric) extends FilterLang
  // TODO: Any other relevant matches?

  trait Acl
  case class Deny(ip: Network.IpPrefix) extends Acl
  case class Permit(ip: Network.IpPrefix) extends Acl

  trait Action
  case object Deny extends Action
  case class Allow(setters: Seq[Setter]) extends Action

  trait Setter
  case class SetLocalPref(lp: Network.LocalPref) extends Setter
  // TODO: Any more setters?

  case class T[M <: Match](matcher: M, action: Action) extends ToZ3[Z3.Expr] {
    override def toZ3: Z3.Expr =
  }
}
*/