package scala.virtualization.lms.regexp

import scala.virtualization.lms.common._

trait Regexp {
  type RE

  val id: RE
  def c(c0: Char): RE
  def in(a: Char, b: Char): RE
  def alt(x: RE, y: RE): RE
  def seq(x: RE, y: RE): RE
  def star(x: RE): RE

  def opt(x: RE): RE = alt(x, id)
  def plus(x: RE): RE =  seq(x, star(x))
  def many(f: (RE, RE) => RE)(x: RE, xs: RE*): RE = xs.length match {
    case 0 => x
    case 1 => f(x, xs(0))
    case n => f(x, many(f)(xs(0), xs.slice(1, n) : _*))
  }
}

trait IfThenElseExpExtra extends IfThenElseExp {
  import scala.reflect.SourceContext
  override def __ifThenElse[T:Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])(implicit pos: SourceContext) =
    if (thenp == elsep) thenp else super.__ifThenElse(cond, thenp, elsep)
}

trait DSLBase extends NumericOps with LiftNumeric with Functions with Equal with OrderingOps with BooleanOps with IfThenElse

trait DSLBaseExp extends NumericOpsExp with LiftNumeric with EqualExpOpt with OrderingOpsExp with BooleanOpsExp with FunctionsExternalDef with CompileScala

trait DSLGenBase extends ScalaGenNumericOps with ScalaGenEqual with ScalaGenOrderingOps with ScalaGenBooleanOps with ScalaGenFunctionsExternal {
  val IR: DSLBaseExp
}
