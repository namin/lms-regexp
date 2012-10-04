package scala.virtualization.lms.regexp

object Main extends App {
  trait Examples extends DSL {
    val aab = many(seq)(star(wildcard), c('A'), c('A'), c('B'))
    val aabany = many(seq)(star(wildcard), c('A'), c('A'), c('B'), star(wildcard))
    val digit = in('0', '9')
    val usd = many(seq)(c('u'), c('s'), c('d'), c(' '), opt(alt(c('+'), c('-'))),
                        plus(digit), c('.'), digit, digit)
    val anything = many(seq)(star(wildcard), opt(c('A')), star(wildcard))
    val cook = seq(star(alt(c('A'), c('B'))), alt(many(seq)(c('A'), c('B'), c('B')), alt(c('A'), c('B'))))
  }

  trait CodeGenerator extends DSL with ImplOpt {
    def output(res: List[(RE, String)]) = {
      val out = new java.io.PrintWriter("benchmark/src/main/scala/LMS.scala")

      for((re,suffix) <- res) {
        codegen.emitAutomata(convertREtoDFA(re), "Match" + suffix, out)
      }

      out.close()
    }
  }
  val exs = new Examples with CodeGenerator
  exs.output(List((exs.aab, "AAB"), (exs.aabany, "AABany"), (exs.usd, "USD"), (exs.anything, "Anything"), (exs.cook, "Cook")))
}

object ParsingMain extends App {
  trait ParsingExamples extends RegexpE {
    val ex1 = alt(many(seq)(g(star(c('a'))), c('a'), c('b')), seq(star(c('a')), g(c('c'))))
  }

  trait ParsingCodeGenerator extends StagedParsing.BitCodedDSL with StagedParsing.BitCodedDSLImpl {
     def output(res: List[(E, String)]) = {
      val out = new java.io.PrintWriter("benchmark/src/main/scala/LMSP.scala")

      for((e,suffix) <- res) {
	val f = (x: Rep[List[Char]]) => {
	  val r = matcher(e)(x)
	  if (r == unit(null)) r else groups(e)(r)
	}
        codegen.emitSource(f, "PMatch" + suffix, out)
      }

      out.close()
    }
  }

  val exs = new ParsingExamples with ParsingCodeGenerator
  exs.output(List((exs.ex1, "1")))
}

object RhinoMain extends App {
  import backtrack._
  val ex1: String = "^((a*ab)|a*(c))$"
  RhinoMatcher.stmatcher.IR.dumpGeneratedCode = true
  val rno = RhinoParser.compileREStub(ex1, "", false)
  val res2 = RhinoMatcher.matchStaged(rno, "", 0)
}
