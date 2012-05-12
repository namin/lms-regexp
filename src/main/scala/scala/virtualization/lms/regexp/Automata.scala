package scala.virtualization.lms.regexp

import scala.virtualization.lms.common._

// careful with @specialized blowup
case class Automaton[@specialized(Boolean,Char,Int) I, @specialized(Boolean,Char,Int) O](out: O, next: I => Automaton[I,O])

trait DFAOps extends Base {

  type DfaState = Automaton[Char,List[Any]]

  type DIO = Rep[DfaState]

  def dfa_flagged(e: Rep[Any])(rec: DIO): DIO
  def dfa_trans(f: Rep[Char] => DIO): DIO = dfa_trans(unit(Nil))(f)
  def dfa_trans(e: Rep[List[Any]])(f: Rep[Char] => DIO): DIO
}


trait DFAOpsExp extends BaseExp with DFAOps { this: Functions => 

  case class DFAFlagged(e: Rep[Any], link: DIO) extends Def[DfaState]
  case class DFAState(e: Rep[List[Any]], f: Rep[Char => DfaState]) extends Def[DfaState]
  
  def dfa_flagged(e: Rep[Any])(rec: DIO): DIO = DFAFlagged(e,rec)
  def dfa_trans(e: Rep[List[Any]])(f: Rep[Char] => DIO): DIO = DFAState(e, doLambda(f))
  
}


trait ScalaGenDFAOps extends ScalaGenBase {
  val IR: DFAOpsExp
  import IR._
  
  override def emitNode(sym: Sym[Any], rhs: Def[Any]) = rhs match {
    case DFAState(e,f) => emitValDef(sym, "scala.virtualization.lms.regexp.Automaton(" + quote(e) + "," + quote(f) + ")")
    case DFAFlagged(e,l) => emitValDef(sym, quote(l) + ".copy(out = " + quote(e) + "::" + quote(l) + ".out)")
    case _ => super.emitNode(sym, rhs)
  }
}


trait NFAtoDFA extends DFAOps { this: NumericOps with LiftNumeric with Functions with Equal with OrderingOps with BooleanOps with IfThenElse =>
  type NIO = List[NTrans]
  
  case class NTrans(c: CharSet, e: Option[Rep[Unit]], s: () => NIO)
  
  def trans(c: CharSet)(s: () => NIO): NIO = List(NTrans(c, None, s))

  def guard(cond: CharSet, found: Boolean = false)(e: => NIO): NIO = {
    List(NTrans(cond, if (found) Some(unit("found").asInstanceOf[Rep[Unit]]) else None, () => e))
  }

  def guards(conds: List[CharSet], found: Boolean = false)(e: => NIO): NIO = {
    conds.flatMap(guard(_, found)(e))
  }

  def stop(): NIO = Nil
  

  sealed abstract class CharSet
  case object W extends CharSet
  case class C(c: Char) extends CharSet
  case class R(a: Char, b: Char) extends CharSet
  
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
    case (R(a1,b1),R(a2,b2)) if a2 <= b1 || a2 <= b2 => Some(s1)
    case (R(a1,b1),C(c2)) if a1 <= c2 && c2 <= b1 => Some(s1)
    case _ => None
  }

  def infix_knowing_not(s1: CharSet, s2: CharSet): Option[CharSet] = (s1,s2) match {
    case (C(c1), C(c2)) if c1 == c2 => None
    case (R(a1,b1),R(a2,b2)) if a2 <= a1 && b1 <= b2 => None
    case (C(c1),R(a2,b2)) if a2 <= c1 && c1 <= b2 => None
    case _ => Some(s1)
  }

  def exploreNFA[A:Manifest](xs: NIO, cin: Rep[Char])(flag: Rep[Any] => Rep[A] => Rep[A])(k: NIO => Rep[A]): Rep[A] = xs match {
    case Nil => k(Nil)
    case NTrans(W, e, s)::rest =>
      val maybeFlag = e map flag getOrElse ((x:Rep[A])=>x)
      maybeFlag(exploreNFA(rest,cin)(flag)(acc => k(acc ++ s())))
    case NTrans(cset, e, s)::rest =>
      if (cset contains cin) {
        val xs1 = for (NTrans(rcset, re, rs) <- rest;
		       kcset <- rcset knowing cset) yield
			 NTrans(kcset,re,rs)
        val maybeFlag = e map flag getOrElse ((x:Rep[A])=>x)
        maybeFlag(exploreNFA(xs1, cin)(flag)(acc => k(acc ++ s())))
      } else {
        val xs1 = for (NTrans(rcset, re, rs) <- rest;
		       kcset <- rcset knowing_not cset) yield
			 NTrans(kcset,re,rs)
        exploreNFA(xs1, cin)(flag)(k)
      }
  }


  def convertNFAtoDFA(in: NIO): DIO = {

    def iterate(state: NIO): DIO = dfa_trans { c: Rep[Char] =>
      exploreNFA(state, c)(dfa_flagged) { next =>
        iterate(next)
      }
    }

    iterate(in)
  }
}

trait DSL extends DFAOps with NFAtoDFA with NumericOps with LiftNumeric with Functions with Equal with OrderingOps with BooleanOps with IfThenElse

trait Impl extends DSL with DFAOpsExp with NumericOpsExp with LiftNumeric with EqualExpOpt with OrderingOpsExp with BooleanOpsExp with IfThenElseExpOpt with IfThenElseFatExp with FunctionsExternalDef with CompileScala { q =>
  override val verbosity = 1
  object codegen extends ScalaGenNumericOps with ScalaGenEqual with ScalaGenOrderingOps with ScalaGenBooleanOps with ScalaGenIfThenElseFat with ScalaGenFunctionsExternal with ScalaGenDFAOps {
    val IR: q.type = q
  }
}
