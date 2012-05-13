package scala.virtualization.lms.regexp

import org.scalatest._

class TestRegexp extends Suite {
  trait Examples extends DSL {
    val aab = many(seq)(star(wildcard), c('A'), c('A'), c('B'))
    // stackoverflow
    // val aabany = many(seq)(star(wildcard), c('A'), c('A'), c('B'), star(wildcard))
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
}
