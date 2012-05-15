package scala.virtualization.lms.regexp

import scala.virtualization.lms.common._

trait DFAOps extends Base {

  type DfaState = Automaton[Char,Boolean]

  type DIO = Rep[DfaState]

  def dfa_trans(f: Rep[Char] => DIO): DIO = dfa_trans(false)(f)
  def dfa_trans(e: Boolean)(f: Rep[Char] => DIO): DIO
}


trait DFAOpsExp extends BaseExp with DFAOps { this: Functions => 

  case class DFAState(e: Boolean, f: Rep[Char => DfaState]) extends Def[DfaState]
  
  def dfa_trans(e: Boolean)(f: Rep[Char] => DIO): DIO = DFAState(e, doLambda(f))
  
}


trait ScalaGenDFAOps extends ScalaGenBase {
  val IR: DFAOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DFAState(true,f) => emitValDef(sym, "scala.virtualization.lms.regexp.Automaton(true," + quote(f) + ")")
    case DFAState(false,f) => emitValDef(sym, "scala.virtualization.lms.regexp.Automaton(false," + quote(f) + ")")
    case _ => super.emitNode(sym, rhs)
  }
}


trait NFAtoDFA extends DFAOps { this: NumericOps with LiftNumeric with Functions with Equal with OrderingOps with BooleanOps with IfThenElse =>
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
  case object W extends CharSet
  case class C(c: Char) extends CharSet
  case class R(a: Char, b: Char) extends CharSet

  def r(a: Char, b: Char) = {
    assert(a <= b)
    if (a == b) C(a) else R(a, b)
  }

  def infix_contains(s: CharSet, c: Rep[Char]): Rep[Boolean] = s match {
    case C(c1) => c == c1
    case R(a, b) => a <= c && c <= b
    case W => unit(true)
  }

  def infix_knowing(s1: CharSet, s2: CharSet): Option[CharSet] = (s1,s2) match {
    case (W,_) => Some(W)
    case (C(c1),C(c2)) if c1 == c2 => Some(W)
    case (R(a1,b1),R(a2,b2)) if a2 <= a1 && b1 <= b2 => Some(W)
    case (C(c1),R(a2,b2)) if a2 <= c1 && c1 <= b2 => Some(W)
    case (R(a1,b1),R(a2,b2)) if a1 <= a2 && a2 <= b1 && b1 <= b2 => Some(r(a2, b1))
    case (R(a1,b1),R(a2,b2)) if a2 <= a1 && a1 <= b2 && b2 <= b1 => Some(r(a1, b2))
    case (R(a1,b1),C(c2)) if a1 <= c2 && c2 <= b1 => Some(s1)
    case _ => None
  }

  def infix_knowing_not(s1: CharSet, s2: CharSet): Option[CharSet] = (s1,s2) match {
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
      exploreNFA(rest,cin)((flag,acc) => k(flag || e(), acc ++ s()))
    case NTrans(cset, e, s)::rest =>
      if (cset contains cin) {
        val xs1 = for (NTrans(rcset, re, rs) <- rest;
		       kcset <- rcset knowing cset) yield
			 NTrans(kcset,re,rs)
        exploreNFA(xs1,cin)((flag,acc) => k(flag || e(), acc ++ s()))
      } else {
        val xs1 = for (NTrans(rcset, re, rs) <- rest;
		       kcset <- rcset knowing_not cset) yield
			 NTrans(kcset,re,rs)
        exploreNFA(xs1, cin)(k)
      }
  }


  def convertNFAtoDFA(in: (NIO, Boolean)): DIO = {

    def iterate(flag: Boolean, state: NIO): DIO = dfa_trans(flag){ c: Rep[Char] =>
      exploreNFA(state, c) { iterate }
    }

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

trait DSL extends DFAOps with NFAtoDFA with RegexpToNFA with NumericOps with LiftNumeric with Functions with Equal with OrderingOps with BooleanOps with IfThenElse

trait Impl extends DSL with DFAOpsExp with NumericOpsExp with LiftNumeric with EqualExpOpt with OrderingOpsExp with BooleanOpsExp with IfThenElseExpOpt with IfThenElseFatExp with FunctionsExternalDef with CompileScala { q =>
  override val verbosity = 1
  object codegen extends ScalaGenNumericOps with ScalaGenEqual with ScalaGenOrderingOps with ScalaGenBooleanOps with ScalaGenIfThenElseFat with ScalaGenFunctionsExternal with ScalaGenDFAOps {
    val IR: q.type = q
  }
}
