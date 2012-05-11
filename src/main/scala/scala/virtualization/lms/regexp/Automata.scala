package scala.virtualization.lms.regexp

import scala.virtualization.lms.common._

// careful with @specialized blowup
case class Automaton[@specialized(Boolean,Char,Int) I, @specialized(Boolean,Char,Int) O](out: O, next: I => Automaton[I,O])

case class NAutomaton[@specialized(Boolean,Char,Int) I, @specialized(Boolean,Char,Int) O](out: O, next: I => List[NAutomaton[I,O]])

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


trait NFAtoDFA extends DFAOps { this: NumericOps with LiftNumeric with Functions with Equal with IfThenElse =>
  type NIO = List[NTrans]
  
  case class NTrans(c: CharSet, e: Option[Rep[Unit]], s: () => NIO)
  
  def trans(c: CharSet)(s: () => NIO): NIO = List(NTrans(c, None, s))

  def guard(cond: CharSet, found: Boolean = false)(e: => NIO): NIO = {
    List(NTrans(cond, if (found) Some(unit("found").asInstanceOf[Rep[Unit]]) else None, () => e))
  }

  def stop(): NIO = Nil
  

  type CharSet = Option[Char]
  
  def infix_contains(s: CharSet, c: Rep[Char]): Rep[Boolean] = s match {
    case Some(c1) => c == c1
    case None => unit(true)
  }

  def infix_intersect(s1: CharSet, s2: CharSet): CharSet = (s1,s2) match {
    case (Some(c1), Some(c2)) if c1 == c2 => None
    case (Some(c1), Some(c2)) => None
    case _ => None
  }

  def infix_diff(s1: CharSet, s2: CharSet): CharSet = (s1,s2) match {
    case (Some(c1), Some(c2)) if c1 == c2 => None
    case (Some(c1), Some(c2)) => None
    case _ => None
  }


  def exploreNFA[A:Manifest](xs: NIO, cin: Rep[Char])(flag: Rep[Any] => Rep[A] => Rep[A])(k: NIO => Rep[A]): Rep[A] = xs match {
    case Nil => k(Nil)
    case NTrans(cset@Some(c), e, s)::rest =>
      if (cset contains cin) {
        val xs1 = rest collect { case NTrans(Some(`c`) | None,e,s) => NTrans(None,e,s) }
        val maybeFlag = e map flag getOrElse ((x:Rep[A])=>x)
        maybeFlag(exploreNFA(xs1, cin)(flag)(acc => k(acc ++ s())))
      } else {
        val xs1 = rest filter { case NTrans(Some(`c`),_,_) => false case _ => true }
        exploreNFA(xs1, cin)(flag)(k)
      }
    case NTrans(None, e, s)::rest =>
      val maybeFlag = e map flag getOrElse ((x:Rep[A])=>x)
      maybeFlag(exploreNFA(rest,cin)(flag)(acc => k(acc ++ s())))
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

trait DSL extends DFAOps with NFAtoDFA with NumericOps with LiftNumeric with Functions with Equal with IfThenElse

trait Impl extends DSL with DFAOpsExp with NumericOpsExp with LiftNumeric with EqualExpOpt with BooleanOpsExp with IfThenElseExpOpt with IfThenElseFatExp with FunctionsExternalDef with CompileScala { q =>
  override val verbosity = 1
  object codegen extends ScalaGenNumericOps with ScalaGenEqual with ScalaGenIfThenElseFat with ScalaGenFunctionsExternal with ScalaGenDFAOps {
    val IR: q.type = q
  }
}
