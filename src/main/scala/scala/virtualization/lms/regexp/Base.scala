package scala.virtualization.lms.regexp

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
