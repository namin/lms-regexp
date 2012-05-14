package scala.virtualization.lms.regexp

import java.io.{PrintStream,File,FileInputStream,FileOutputStream,ByteArrayOutputStream}
import org.scalatest._

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
}

trait FileDiffSuite extends Suite {
  val prefix = "test-out/"

  def withOutFile(name: String)(func: => Unit): Unit = {
    val file = new File(name)
    file.getParentFile.mkdirs()
    withOutput(new PrintStream(new FileOutputStream(file)))(func)
  }
  def captureOutput(func: => Unit): String = {
    val bstream = new ByteArrayOutputStream
    withOutput(new PrintStream(bstream))(func)
    bstream.toString
  }
  def withOutput(out: PrintStream)(func: => Unit): Unit = {
    val oldStdOut = System.out
    val oldStdErr = System.err
    try {
      System.setOut(out)
      System.setErr(out)
      Console.withOut(out)(Console.withErr(out)(func))
    } finally {
      out.flush()
      out.close()
      System.setOut(oldStdOut)
      System.setErr(oldStdErr)
    }
  }
  
  def readFile(name: String): String = {
    val buf = new Array[Byte](new File(name).length().toInt)
    val fis = new FileInputStream(name)
    fis.read(buf)
    fis.close()
    new String(buf)
  }
  def assertFileEqualsCheck(name: String): Unit = {
    expect(readFile(name+".check")){readFile(name)}
    new File(name) delete ()
  }
}
