package scala.virtualization.lms.regexp

import org.scalatest._

class TestRegexp extends Suite {
  trait Examples extends DSL {
    val aab = many(seq)(star(wildcard), c('A'), c('A'), c('B'))
    val aabx = many(seq)(star(wildcard), c('A'), c('A'), c('B'), star(c('X')))
    val aabany = many(seq)(star(wildcard), c('A'), c('A'), c('B'), star(wildcard))
    val digit = in('0', '9')
    val usd = many(seq)(c('u'), c('s'), c('d'), c(' '), opt(alt(c('+'), c('-'))),
                        plus(digit), c('.'), digit, digit)
    val fool = alt(seq(wildcard, opt(c('B'))), seq(wildcard, opt(c('A'))))
    val fool2 = many(alt)(seq(wildcard, opt(c('B'))), seq(wildcard, opt(c('C'))), seq(wildcard, c('A')))
    val fool3 = many(alt)(seq(wildcard, opt(c('B'))), seq(wildcard, opt(c('C'))), seq(c('X'), c('A')))
    val any = many(seq)(star(wildcard), opt(c('A')), star(wildcard))
  }

  trait Evaluator extends DSL with Impl {
    def recompile(re: RE) = {
      val f = (x: Rep[Unit]) => convertREtoDFA(re)
      val fc = compile(f)
      fc
    }
    def begmatch(fc: Unit => DfaState)(input: String): Boolean = {
      var state = fc()
      var i = 0
      val n = input.length
      while ((state.out % 2 != 1) && (i < n)) {
        state = state.next(input.charAt(i))
        i += 1
      }
      state.out % 2 == 1
    }
    def fullmatch(fc: Unit => DfaState)(input: String): Boolean = {
      var state = fc()
      var i = 0
      val n = input.length
      while ((state.out & 2) != 2 && (i < n)) {
        state = state.next(input.charAt(i))
        i += 1
      }
      state.out % 2 == 1
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
    expect(false){exs.begmatch(fc)("")}
    expect(false){exs.fullmatch(fc)("")}
  }

  def testAABX = {
    val exs = new Examples with Evaluator
    val fc = exs.recompile(exs.aabx)

    expect(true){exs.fullmatch(fc)("AAB")}
    expect(true){exs.fullmatch(fc)("XYZAAB")}
    expect(true){exs.fullmatch(fc)("XYZABAAB")}
    expect(true){exs.fullmatch(fc)("XYZAABX")}
    expect(true){exs.begmatch(fc)("XYZAABX")}
    expect(false){exs.begmatch(fc)("XYZABX")}
    expect(false){exs.begmatch(fc)("")}
    expect(false){exs.fullmatch(fc)("")}
  }

  def testAABany = {
    val exs = new Examples with Evaluator
    val fc = exs.recompile(exs.aabany)

    expect(true){exs.fullmatch(fc)("AAB")}
    expect(true){exs.fullmatch(fc)("XYZAAB")}
    expect(true){exs.fullmatch(fc)("XYZABAAB")}
    expect(true){exs.fullmatch(fc)("XYZAABX")}
    expect(true){exs.fullmatch(fc)("XYZAABXYZ")}
    expect(true){exs.begmatch(fc)("XYZAABX")}
    expect(false){exs.begmatch(fc)("XYZABX")}
    expect(false){exs.begmatch(fc)("")}
    expect(false){exs.fullmatch(fc)("")}
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
    expect(false){exs.begmatch(fc)("")}
    expect(false){exs.fullmatch(fc)("")}
  }

  def testFool = {
    val exs = new Examples with Evaluator
    val fc = exs.recompile(exs.fool)

    expect(false){exs.begmatch(fc)("")}
    expect(false){exs.fullmatch(fc)("")}
    expect(true){exs.fullmatch(fc)("X")}
    expect(true){exs.fullmatch(fc)("XA")}
    expect(true){exs.fullmatch(fc)("XB")}
    expect(false){exs.fullmatch(fc)("XC")}
  }

  def testFool2 = {
    val exs = new Examples with Evaluator
    val fc = exs.recompile(exs.fool2)

    expect(false){exs.begmatch(fc)("")}
    expect(false){exs.fullmatch(fc)("")}
    expect(true){exs.fullmatch(fc)("X")}
    expect(true){exs.fullmatch(fc)("XA")}
    expect(true){exs.fullmatch(fc)("XB")}
    expect(true){exs.fullmatch(fc)("XC")}
    expect(false){exs.fullmatch(fc)("XD")}
  }

  def testFool3 = {
    val exs = new Examples with Evaluator
    val fc = exs.recompile(exs.fool3)

    expect(false){exs.begmatch(fc)("")}
    expect(false){exs.fullmatch(fc)("")}
    expect(true){exs.fullmatch(fc)("X")}
    expect(true){exs.fullmatch(fc)("XA")}
    expect(true){exs.fullmatch(fc)("XB")}
    expect(true){exs.fullmatch(fc)("XC")}
    expect(false){exs.fullmatch(fc)("YA")}
    expect(true){exs.fullmatch(fc)("YB")}
    expect(true){exs.fullmatch(fc)("YC")}
    expect(false){exs.fullmatch(fc)("XD")}
  }

  def testAny = {
    val exs = new Examples with Evaluator
    val fc = exs.recompile(exs.any)

    expect(true){exs.begmatch(fc)("")}
    expect(true){exs.fullmatch(fc)("")}
    expect(true){exs.begmatch(fc)("X")}
    expect(true){exs.fullmatch(fc)("X")}
    expect(true){exs.begmatch(fc)("XX")}
    expect(true){exs.fullmatch(fc)("XX")}
  }
}
