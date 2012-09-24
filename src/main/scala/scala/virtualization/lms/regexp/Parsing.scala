package scala.virtualization.lms.regexp

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

/**
 * Bit-coded Regular Expression Parsing
 * by Lasse Nielsen and Fritz Henglein
 */
object BitCoded {
  type Code = List[Bit]
  type Bit = Boolean
  val c0: Bit = false
  val c1: Bit = true
  val epsilon: Code = List()

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
