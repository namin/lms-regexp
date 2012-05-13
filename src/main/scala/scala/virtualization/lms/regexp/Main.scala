package scala.virtualization.lms.regexp

object Main extends App {
  trait Examples extends DSL {
    val aab = many(seq)(star(wildcard), c('A'), c('A'), c('B'))
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
  }
  def fullmatch(fc: Unit => Automaton[Char,List[Any]])(input: String): Boolean = {
    var state = fc()
    input.foreach(c => state = state.next(c))
    !state.out.isEmpty
  }
  val exs = new Examples with Evaluator
  val regexps = Array(
    (exs.recompile(exs.aab), ".*AAB"),
    (exs.recompile(exs.usd), "usd [+-]?[0-9]+.[0-9][0-9]"))
  val inputs = Array(
        "AAB",
        "hello AAB",
        "AAB no",
        "http://www.linux.com/",
        "http://www.thelinuxshow.com/main.php3",
        "usd 1234.00",
        "he said she said he said no",
        "same same same",
        "{1:\n" + "this is some more text - and some more and some more and even more\n" +
        //(1 to 40).map(_ => "this is some more text and some more and some more and even more\n").mkString +
        "this is some more text and some more and some more and even more at the end\n" + "-}\n"
  )
  val expected = Array(
    Array(true, true, false, false, false, false, false, false, false),
    Array(false, false, false, false, false, true, false, false, false))
  val ITERATIONS = 100000
  val debug = true

  val autoRegexps = for ((other,re) <- regexps) yield {
    val regexp = new dk.brics.automaton.RegExp(re)
    val auto = regexp.toAutomaton()
    val runauto = new dk.brics.automaton.RunAutomaton(auto, true)
    (other, runauto)
  }

  for (ri <- 0 to regexps.length-1; (lms,dk) = autoRegexps(ri)) {
      for (i <- 0 to inputs.length-1; input = inputs(i)) {
        val dkResult = dk.run(input)
        val lmsResult = fullmatch(lms)(input)
        assert(dkResult == lmsResult)

        val expectedResult = expected(ri)(i)
        assert(dkResult == expectedResult)
        assert(lmsResult == expectedResult)
      }
  }

  println("Testing java.util.regex ...")
  val javaRegexps = for((_,re) <- regexps) yield java.util.regex.Pattern.compile(re)
  val javaStartTime = System.currentTimeMillis()
  for (re <- javaRegexps) {
    for (iter <- 1 to ITERATIONS) {
      for (input <- inputs) {
        val result = re.matcher(input).find()
      }
    }
  }
  val javaEndTime = System.currentTimeMillis()
  println("... " + (javaEndTime - javaStartTime))

  println("Testing dk.brics.automaton ...")
  val dkStartTime = System.currentTimeMillis()
  for ((_,runauto) <- autoRegexps) {
    for (iter <- 1 to ITERATIONS) {
      for (input <- inputs) {
        val result = runauto.run(input)
      }
    }
  }
  val dkEndTime = System.currentTimeMillis()
  println("... " + (dkEndTime - dkStartTime))

  println("Testing lms-regexp ...")
  val lmsStartTime = System.currentTimeMillis()
  for ((fc,_) <- regexps) {
    for (iter <- 1 to ITERATIONS) {
      for (input <- inputs) {
        val result = fullmatch(fc)(input)
      }
    }
  }
  val lmsEndTime = System.currentTimeMillis()
  println("... " + (lmsEndTime - lmsStartTime))
}
