package scala.virtualization.lms.regexp

class TestTrans extends FileDiffSuite {
  trait Examples extends DSL {
    val aab = many(seq)(star(wildcard), c('A'), c('A'), c('B'))
    val aabany = many(seq)(star(wildcard), c('A'), c('A'), c('B'), star(wildcard))
  }

  trait Evaluator extends DSL with ImplTrans {
    def recompile(re: RE) = {
      val f = codegen.pack(convertREtoDFA(re))
      val fc = compile(f)
      fc
    }
  }

  trait Go extends DSL with ImplTrans {
    def go(name: String, re: RE) = {
      val fn = prefix+"trans_"+name
      withOutFile(fn) {
        codegen.emitAutomata(convertREtoDFA(re), "Match", new java.io.PrintWriter(System.out))
      }
      assertFileEqualsCheck(fn)
    }
  }

  def testEvalAAB = {
    val exs = new Examples with Evaluator
    val fc = exs.recompile(exs.aab)

    expect(true){fc("AAB")}
    expect(true){fc("XYZAAB")}
    expect(true){fc("XYZABAAB")}
    expect(false){fc("XYZAABX")}
    expect(false){fc("")}
  }

  def testEvalAABany = {
    val exs = new Examples with Evaluator
    val fc = exs.recompile(exs.aabany)

    expect(true){fc("AAB")}
    expect(true){fc("XYZAAB")}
    expect(true){fc("XYZABAAB")}
    expect(true){fc("XYZAABX")}
    expect(false){fc("XYZABX")}
    expect(false){fc("")}
  }

  def testCompileAAB = {
    val exs = new Examples with Go
    exs.go("aab", exs.aab)
  }

  def testCompileAABany = {
    val exs = new Examples with Go
    exs.go("aabany", exs.aabany)
  }

}
