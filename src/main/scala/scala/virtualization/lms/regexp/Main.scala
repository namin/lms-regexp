package scala.virtualization.lms.regexp

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

  trait EmptyProg extends DSL {
    def test(x: Rep[Unit]) = convertNFAtoDFA(Nil)
  }

  trait NProg extends DSL {
    def test(x: Rep[Unit]) = {
      def findAAB(): NIO = {
        guards(List(Some('A'), Some('X'))) {
          guard(Some('A')) {
            guards(List(Some('A'), Some('B')), true) {
              stop()
        }}} ++
        guard(None) { findAAB() } // in parallel ...
      }
      convertNFAtoDFA(findAAB())
    }
  }

  trait Go extends Impl {
    def test(x:Rep[Unit]): DIO

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

  val prog = new Prog with Go
  prog.go()

  val emptyProg = new EmptyProg with Go
  emptyProg.go()

  val nprog = new NProg with Go
  nprog.go()
}
