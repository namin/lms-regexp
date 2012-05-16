package scala.virtualization.lms.regexp

object Main extends App {
  trait Examples extends DSL {
    val aab = many(seq)(star(wildcard), c('A'), c('A'), c('B'))
    val digit = in('0', '9')
    val usd = many(seq)(c('u'), c('s'), c('d'), c(' '), opt(alt(c('+'), c('-'))),
                        plus(digit), c('.'), digit, digit)
    val anything = many(seq)(star(wildcard), opt(c('A')), star(wildcard))
  }
  trait CodeGenerator extends DSL with Impl {
    def output(res: List[(RE, String)]) = {
      val out = new java.io.PrintWriter("benchmark/src/main/scala/LMS.scala")

      for((re,suffix) <- res) {
        val f = (x: Rep[Unit]) => convertREtoDFA(re)
        codegen.emitSource(f, "Match" + suffix, out)
      }

      out.close()
    }
  }
  val exs = new Examples with CodeGenerator
  exs.output(List((exs.aab, "AAB"), (exs.usd, "USD"), (exs.anything, "Anything")))
}
