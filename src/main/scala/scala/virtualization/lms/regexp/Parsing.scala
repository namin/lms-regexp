package scala.virtualization.lms.regexp

// Bit-coded Regular Expression Parsing by Lasse Nielsen and Fritz Henglein
// http://www.diku.dk/hjemmesider/ansatte/henglein/papers/henglein2011b.pdf

sealed abstract class V { v =>
  val e : E { type T >: v.type }
}
case class V0() extends V { val e = E0 }
case class V1() extends V { val e = E1 }
case class Vchar(c: Char) extends V { val e = Echar(c) }
sealed abstract class Vplus(e1: E, e2: E) extends V { val e = Eplus(e1, e2) }
case class Vinl(v1: V, e2: E) extends Vplus(v1.e, e2)
case class Vinr(e1: E, v2: V) extends Vplus(e1, v2.e)
case class Vprod(v1: V, v2: V) extends V { val e = Eprod(v1.e, v2.e) }
case class Vstar(vs: List[V])(es: E) extends V { val e = Estar(es) }

sealed abstract class E { type T <: V }
case object E0 extends E { type T = V0 }
case object E1 extends E { type T = V1 }
case class Echar(c: Char) extends E { type T = Vchar }
case class Eplus(e1: E, e2: E) extends E { type T = Vplus }
case class Eprod(e1: E, e2: E) extends E { type T = Vprod }
case class Estar(es: E) extends E { type T = Vstar }

object BitCoded {
  type Code = List[Bit]
  type Bit = Boolean
  val c0: Bit = false
  val c1: Bit = true
  val epsilon: Code = List()

  def one(c: Bit): Code = List(c)

  def str(c: Code): String = {
    c.map(if (_) "1" else "0").mkString
  }

  def unstr(s: String): Code = {
    s.map(c => if (c == '0') c0 else c1).toList
  }

  def code(v : V): Code = v match {
    case V0() => epsilon
    case V1() => epsilon
    case Vchar(c) => epsilon
    case Vinl(v1, e2) => c0 :: code(v1)
    case Vinr(e1, v2) => c1 :: code(v2)
    case Vprod(v1, v2) => code(v1) ++ code(v2)
    case Vstar(vs) => vs.flatMap(v => c0 :: code(v)) :+ c1
  }

  def decode(e: E)(d: Code): V = {
    def rec(e : E)(d: Code): (V, Code) = (e,d) match {
      case (E0, d) => (V0(), d)
      case (E1, d) => (V1(), d)
      case (Echar(c), d) => (Vchar(c), d)
      case (Eplus(e1, e2), `c0`::d) =>
        val (v1, dr) = rec(e1)(d)
        (Vinl(v1, e2), dr)
      case (Eplus(e1, e2), `c1`::d) =>
	val (v2, dr) = rec(e2)(d)
	(Vinr(e1, v2), dr)
      case (Eprod(e1, e2), d) =>
        val (v1, dr) = rec(e1)(d)
        val (v2, drr) = rec(e2)(dr)
        (Vprod(v1, v2), drr)
      case (Estar(es), `c0`::d) =>
        val (vh,dr) = rec(es)(d)
        val (Vstar(vt),drr) = rec(e)(dr)
        (Vstar(vh :: vt)(es), drr)
      case (Estar(es), `c1`::d) => (Vstar(List())(es), d)
    }
    val (v, `epsilon`) = rec(e)(d)
    v
  }
}

object Parsing {
  import BitCoded._

  case class NTr(fromState: Int, toState: Int, input: Option[Char], output: List[Bit]) {
    def mapStates(f: Int => Int) = NTr(f(fromState), f(toState), input, output)
  }
  case class NFA(nStates: Int, transitions: Set[NTr])

  object NFA {
    def mapStates(ts: Set[NTr], f: Int => Int): Set[NTr] = ts.map(_.mapStates(f))
    def inout(n: Int, f: Int => Int = s => s): (Int,Int) = (f(0), f(n-1))

    def fromE(e: E): NFA = e match {
      case E0 => NFA(2, Set())
      case E1 => NFA(1, Set())
      case Echar(c) =>
        val (in0,out0) = inout(2)
        NFA(2, Set(NTr(in0, out0, Some(c), epsilon)))
      case Eprod(e1, e2) =>
        val NFA(n1, t1) = fromE(e1)
        val NFA(n2, t2) = fromE(e2)
        val f2 = (s: Int) => s + n1 - 1
        NFA(n1 + n2 - 1,t1 ++ mapStates(t2, f2))
      case Eplus(e1, e2) =>
        val NFA(n1, t1) = fromE(e1)
        val NFA(n2, t2) = fromE(e2)
        val f1 = (s: Int) => s + 1
        val f2 = (s: Int) => s + 1 + n1
        val (in1, out1) = inout(n1, f1)
        val (in2, out2) = inout(n2, f2)
        val n = n1 + n2 + 2
        val (in0,out0) = inout(n)
        NFA(n, mapStates(t1, f1) ++ mapStates(t2, f2) ++ Set(
	  NTr(in0, in1, None, one(c0)),
	  NTr(in0, in2, None, one(c1)),
	  NTr(out1, out0, None, epsilon),
	  NTr(out2, out0, None, epsilon)
	))
      case Estar(es) =>
        val NFA(ns,ts) = fromE(es)
        val fs = (s: Int) => s + 1
        val n = ns + 2
        val (ins,outs) = inout(ns, fs)
        val (in0,out0) = inout(n)
        NFA(n, mapStates(ts, fs) ++ Set(
	  NTr(in0, ins, None, one(c0)),
	  NTr(outs, in0, None, epsilon),
	  NTr(in0, out0, None, one(c1))
	))
      case _ => ???
    }

    def run(nfa: NFA)(str: List[Char]): Set[Code] = {
      val NFA(n, t) = nfa
      val (in0,out0) = inout(n)
      var paths = Set((in0, str, epsilon))
      var results = Set[Code]()
      while (!paths.isEmpty) {
	results ++= paths.collect{case (`out0`, Nil, output) => output.reverse}
	paths = paths.filter{case (s,cs,d) => !cs.isEmpty}.flatMap{case (s,c::cs,d) => t.collect{case NTr(`s`, toState, Some(`c`), output) => (toState, cs, output ++ d)}} ++ paths.flatMap{case (s,cs,d) => t.collect{case NTr(`s`, toState, None, output) => (toState, cs, output ++ d)}}
      }
      return results
    }
  }

  type NState = Int
  type DState = Int
  case class DTr(fromState: DState, toState: DState, input: Char, outputMap: Map[NState, (NState, Code)])
  case class DFA(nStates: Int, finals: Set[DState], transitions: Set[DTr], stateMap: Map[DState, Set[NState]], initMap: Map[NState, Code])

  object DFA {
    def fromNFA(nfa: NFA): DFA = {
      ???
    }

    // adapted from http://www.cs.uaf.edu/~cs631/notes/strings/aho2ed/Fig3_33.gif
    def epsilonClosures(nfa: NFA): Map[NState, Map[NState, Code]] = {
      var result = Map[NState, Map[NState, Code]]()
      var stack = collection.immutable.Stack[(NState, NState)]()
      for (t <- 0 to nfa.nStates-1) {
        result = result.updated(t, Map(t -> epsilon))
        stack = stack.push((t,t))
      }

      while (!stack.isEmpty) {
        val (t,root) = stack.top
        stack = stack.pop

        for (tr <- nfa.transitions;
          if tr.fromState == t;
          if tr.input == None;
          u = tr.toState;
          if !result(root).contains(u);
          o = tr.output) {
          val rootMap = result(root)
          val c = rootMap(t) ++ o
          result = result.updated(root, rootMap.updated(u, c))
          stack = stack.push((u,root))
        }
      }

      result
    }
  }
}
