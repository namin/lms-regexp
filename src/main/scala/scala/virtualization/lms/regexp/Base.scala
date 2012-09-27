package scala.virtualization.lms.regexp

trait Regexp {
  type RE

  val id: RE
  def c(c0: Char): RE
  def in(a: Char, b: Char): RE
  def alt(x: RE, y: RE): RE
  def seq(x: RE, y: RE): RE
  def star(x: RE): RE
  def opt(x: RE): RE

  def plus(x: RE): RE = {
    seq(x, star(x))
  }
  def many(f: (RE, RE) => RE)(xs: RE*): RE = xs.length match {
    case 0 => id
    case 1 => xs(0)
    case 2 => f(xs(0), xs(1))
    case n => f(xs(0), many(f)(xs.slice(1, n) : _*))
  }
}
