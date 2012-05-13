package scala.virtualization.lms.regexp

import org.scalatest._

class TestRegexp extends Suite {
  trait Examples extends DSL {
    val aab = many(seq)(star(wildcard), c('A'), c('A'), c('B'))
    // stackoverflow
    // val aabany = many(seq)(star(wildcard), c('A'), c('A'), c('B'), star(wildcard))
    val digit = in('0', '9')
    val usd = many(seq)(c('u'), c('s'), c('d'), c(' '), opt(alt(c('+'), c('-'))),
                        plus(digit), c('.'), digit, digit)
  }
  trait Evaluator extends DSL with Impl {
    def recompile(re: RE) = {
      val f = (x: Rep[Unit]) => convertREtoDFA(re)
      val fc = compile(f)
      fc
    }
    def begmatch(fc: Unit => DfaState)(input: String): Boolean = {
      var state = fc()
      var found = false
      def update() = found = found || !state.out.isEmpty
      update()
      input.foreach { c =>
        state = state.next(c)
        update()
      }
      found
    }
    def fullmatch(fc: Unit => DfaState)(input: String): Boolean = {
      var state = fc()
      input.foreach(c => state = state.next(c))
      !state.out.isEmpty
    }
  }

  def testAAB = {
    val exs = new Examples with Evaluator
    val fc = exs.recompile(exs.aab)

    expect(true){exs.fullmatch(fc)("AAB")}
    expect(true){exs.fullmatch(fc)("XYZAAB")}
    expect(true){exs.fullmatch(fc)("XYZABAAB")}
    expect(false){exs.fullmatch(fc)("XYZAABX")}
    expect(true){exs.begmatch(fc)("XYZAABX")}
    expect(false){exs.begmatch(fc)("XYZABX")}
  }

  def testUSD = {
    val exs = new Examples with Evaluator
    val fc = exs.recompile(exs.usd)

    expect(true){exs.fullmatch(fc)("usd 1234.00")}
    expect(true){exs.fullmatch(fc)("usd 1234.01")}
    expect(false){exs.fullmatch(fc)("usd 1234.01  ")}
    expect(true){exs.begmatch(fc)("usd 1234.01  ")}
    expect(false){exs.fullmatch(fc)("usd1234.00")}
    expect(false){exs.fullmatch(fc)("usd 1234")}
    expect(false){exs.begmatch(fc)("  usd 1234.01  ")}

  }
}
