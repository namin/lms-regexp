import scala.virtualization.lms.regexp.Automaton
import RegexpType._

trait DFAOps {
  type Rep[T] = T
  def unit[T](x: T) = x

  type DfaState = Automaton[Char,Boolean]

  type DIO = Rep[DfaState]

  def dfa_trans(f: Rep[Char] => DIO): DIO = dfa_trans(false)(f)
  def dfa_trans(e: Boolean)(f: Rep[Char] => DIO): DIO = Automaton(e, f)
}

trait NFAtoDFA extends DFAOps {
  type NIO = List[NTrans]
  
  case class NTrans(c: CharSet, e: () => Boolean, s: () => NIO)
  
  def trans(c: CharSet)(s: () => NIO): NIO = List(NTrans(c, () => false, s))

  def guard(cond: CharSet, found: => Boolean = false)(e: => NIO): NIO = {
    List(NTrans(cond, () => found, () => e))
  }

  def guards(conds: List[CharSet], found: Boolean = false)(e: => NIO): NIO = {
    conds.flatMap(guard(_, found)(e))
  }

  def stop(): NIO = Nil
  

  sealed abstract class CharSet
  case class R(a: Char, b: Char) extends CharSet
  case class C(c: Char) extends CharSet
  case object W extends CharSet

  def r(a: Char, b: Char) = {
    assert(a <= b)
    if (a == b) C(a) else R(a, b)
  }

  def contains(s: CharSet, c: Rep[Char]): Rep[Boolean] = s match {
    case C(c1) => c == c1
    case R(a, b) => a <= c && c <= b
    case W => unit(true)
  }

  def knowing(s1: CharSet, s2: CharSet): Option[CharSet] = (s1,s2) match {
    case (W,_) => Some(W)
    case (C(c1),C(c2)) if c1 == c2 => Some(W)
    case (R(a1,b1),R(a2,b2)) if a2 <= a1 && b1 <= b2 => Some(W)
    case (C(c1),R(a2,b2)) if a2 <= c1 && c1 <= b2 => Some(W)
    case (R(a1,b1),R(a2,b2)) if a1 <= a2 && a2 <= b1 && b1 <= b2 => Some(r(a2, b1))
    case (R(a1,b1),R(a2,b2)) if a2 <= a1 && a1 <= b2 && b2 <= b1 => Some(r(a1, b2))
    case (R(a1,b1),C(c2)) if a1 <= c2 && c2 <= b1 => Some(s1)
    case _ => None
  }

  def knowing_not(s1: CharSet, s2: CharSet): Option[CharSet] = (s1,s2) match {
    case (C(c1), C(c2)) if c1 == c2 => None
    case (R(a1,b1),R(a2,b2)) if a2 <= a1 && b1 <= b2 => None
    case (C(c1),R(a2,b2)) if a2 <= c1 && c1 <= b2 => None
    case (R(a1,b1),R(a2,b2)) if a1 <= a2 && a2 <= b1 && b1 <= b2 => Some(r(a1, a2))
    case (R(a1,b1),R(a2,b2)) if a2 <= a1 && a1 <= b2 && b2 <= b1 => Some(r(b2, b1))
    case _ => Some(s1)
  }

  def exploreNFA[A:Manifest](xs: NIO, cin: Rep[Char])(k: (Boolean, NIO) => Rep[A]): Rep[A] = xs match {
    case Nil => k(false, Nil)
    case NTrans(W, e, s)::rest =>
      val (xs1, xs2) = xs.partition(_.c != W)
      exploreNFA(xs1,cin)((flag,acc) => k(flag || xs2.exists(_.e()), acc ++ xs2.flatMap(_.s())))
    case NTrans(cset, e, s)::rest =>
      if (contains(cset, cin)) {
        val xs1 = for (NTrans(rcset, re, rs) <- rest;
		       kcset <- knowing(rcset, cset)) yield
			 NTrans(kcset,re,rs)
        exploreNFA(xs1,cin)((flag,acc) => k(flag || e(), acc ++ s()))
      } else {
        val xs1 = for (NTrans(rcset, re, rs) <- rest;
		       kcset <- knowing_not(rcset, cset)) yield
			 NTrans(kcset,re,rs)
        exploreNFA(xs1, cin)(k)
      }
  }


  def convertNFAtoDFA(in: (NIO, Boolean)): DIO = {

    def iterate(flag: Boolean, state: NIO): DIO = {
      dfa_trans(flag){ c: Rep[Char] => exploreNFA(state, c) { iterate }
    }}

    iterate(in._2, in._1)
  }
}

trait RegexpToNFA { this: NFAtoDFA =>
  type RE = (() => (NIO, Boolean)) => (NIO, Boolean)

  def wrap(cset: CharSet): RE = { nio: (() => (NIO, Boolean)) =>
    (guard(cset, nio()._2)(nio()._1), false)
  }

  def c(c0: Char): RE = wrap(C(c0))
  def in(a: Char, b: Char): RE = wrap(r(a, b))
  val wildcard: RE = wrap(W)

  def alt(x: RE, y: RE): RE = { nio: (() => (NIO, Boolean)) =>
    val (nx, ex) = x(nio)
    val (ny, ey) = y(nio)
    (nx ++ ny, ex || ey)
  }

  def seq(x: RE, y: RE): RE = { nio: (() => (NIO, Boolean)) =>
    x(() => y(nio))
  }

  val id = {nio: (() => (NIO, Boolean)) => nio()}

  def many(f: (RE, RE) => RE)(xs: RE*): RE = xs.length match {
    case 0 => id
    case 1 => xs(0)
    case 2 => f(xs(0), xs(1))
    case n => f(xs(0), many(f)(xs.slice(1, n) : _*))
  }

  def star(x: RE): RE = { nio: (() => (NIO, Boolean)) =>
    val (nn, en) = nio()
    def rec(): (NIO, Boolean) = {
      val (nx, ex) = x(rec)
      (nn ++ nx, en || ex)
    }
    rec()
  }

  def plus(x: RE): RE = {
    seq(x, star(x))
  }

  def opt(x: RE): RE = { nio: (() => (NIO, Boolean)) =>
    val (nn, en) = nio()
    val (nx, ex) = x(nio)
    (nn ++ nx, en || ex)
  }

  def convertREtoDFA(re: RE): DIO = convertNFAtoDFA(re(() => (Nil, true)))
}

class MatcherUnstaged(regexp: RegexpType) extends RegexpMatcher(regexp) with RegexpToNFA with NFAtoDFA {
  private val re: RE = regexp match {
    case ANY_AAB =>
      many(seq)(star(wildcard), c('A'), c('A'), c('B'))
    case ANY_AAB_ANY =>
      many(seq)(star(wildcard), c('A'), c('A'), c('B'), star(wildcard))
    case USD =>
      val digit = in('0', '9')
      many(seq)(c('u'), c('s'), c('d'), c(' '), opt(alt(c('+'), c('-'))), plus(digit), c('.'), digit, digit)
    case COOK =>
      seq(star(alt(c('A'), c('B'))), alt(many(seq)(c('A'), c('B'), c('B')), alt(c('A'), c('B'))))
    case ANY =>
      many(seq)(star(wildcard), opt(c('A')), star(wildcard))
  }
  private val initialState = convertREtoDFA(re)

  override def matches(input: String): Boolean = {
    var state = initialState
    var i = 0
    val n = input.length
    while (i < n) {
      state = state.next(input.charAt(i))
      i += 1
    }
    state.out
  }
}
