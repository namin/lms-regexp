package scala.virtualization.lms.regexp

class TestCodeGeneration extends FileDiffSuite {
  trait Go extends Impl {
    def test(x: Rep[Unit]): DIO
    def go(name: String) = {
      withOutFile(prefix+name) {
        val f = (x: Rep[Unit]) => test(x)
        codegen.emitSource(f, "Match", new java.io.PrintWriter(System.out))
        val fc = compile(f)

        val input = List('X','A','B','Z','A','A','B','W','A','A','A','A','B','Q')
        def run() {
          var state = fc()

          var idx = 0
          input foreach { c =>
            println("// idx:   " + idx)
            println("// out:   " + state.out)
            println("// char:  " + c)

            idx += 1
            state = state.next(c)
          }
      
          println("// idx:   " + idx)
          println("// out:   " + state.out)
        }
        run()
      }
      assertFileEqualsCheck(prefix+name)
    }
  }

  trait GuardProg extends DSL {
    def test(x: Rep[Unit]) = {
      def findAAB(): NIO = {
        guard(C('A')) {
          guard(C('A')) {
            guard(C('B'), true) {
              stop()
        }}} ++
        guard(W) { findAAB() } // in parallel ...
      }
      convertNFAtoDFA((findAAB(), false))
    }
  }
  def testGuardProg = (new GuardProg with Go).go("aab")

  trait RegexpProg extends DSL {
    def test(x: Rep[Unit]) = convertREtoDFA(
      many(seq)(star(wildcard), c('A'), c('A'), c('B'))
    )
  }
  def testRegexpProg = (new RegexpProg with Go).go("aab")

  trait SORegexpProg extends DSL {
    def test(x: Rep[Unit]) = convertREtoDFA(
      many(seq)(star(wildcard), c('A'), c('A'), c('B'), star(wildcard))
    )
  }
  def testSORegexpProg = (new SORegexpProg with Go).go("aabany")
}
