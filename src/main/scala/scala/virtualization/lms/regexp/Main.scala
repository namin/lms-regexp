package scala.virtualization.lms.regexp

import scala.virtualization.lms.common._

object Main extends App {

  trait Prog extends DSL {
    def test(x: Rep[Unit]) = {
      def findAAB(): NIO = {
        guard(Some('A')) {
          guard(Some('A')) {
            guard(Some('B'), true) {
              stop()
        }}} ++
        guard(None) { findAAB() } // in parallel ...
      }
      convertNFAtoDFA(findAAB())
    }
  }

  val prog = new Prog with Impl {
    def go() = {
      val f = (x:Rep[Unit]) => test(x)
      codegen.emitSource(f, "Match", new java.io.PrintWriter(System.out))
      val fc = compile(f)

      val input = List('X','A','B','Z','A','A','B','W','A','A','A','A','B','Q')
      def run() {
        var state = fc()

        var idx = 0
        input foreach { c =>
          println("idx:   " + idx)
          println("out:   " + state.out)
          println("char:  " + c)

          idx += 1
          state = state.next(c)
        }
      
        println("idx:   " + idx)
        println("out:   " + state.out)
      }
      run()
    }
  }

  prog.go()
}
