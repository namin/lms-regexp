object Main extends App {
  def fullmatch(fc: Unit => scala.virtualization.lms.regexp.Automaton[Char,Boolean])(input: String): Boolean = {
    var state = fc()
    var i = 0
    val n = input.length
    while (i < n) {
      state = state.next(input(i))
      i += 1
    }
    state.out
  }
  val regexps = Array(
    (new MatchAAB, ".*AAB"),
    (new MatchUSD, "usd [+-]?[0-9]+.[0-9][0-9]"))
  val inputs = Array(
        "AAB",
        "hello AAB",
        "AAB no",
        "http://www.linux.com/",
        "http://www.thelinuxshow.com/main.php3",
        "usd 1234.00",
        "   usd 1234.00",
        "he said she said he said no",
        "same same same",
        "{1:\n" + "this is some more text - and some more and some more and even more\n" +
        //(1 to 40).map(_ => "this is some more text and some more and some more and even more\n").mkString +
        "this is some more text and some more and some more and even more at the end\n" + "-}\n"
  )
  val expected = Array(
    Array(true, true, false, false, false, false, false, false, false, false),
    Array(false, false, false, false, false, true, false, false, false, false))
  val ITERATIONS = 100000
  val debug = true

  val allRegexps = for ((other,re) <- regexps) yield {
    val regexp = new dk.brics.automaton.RegExp(re)
    val auto = regexp.toAutomaton()
    val runauto = new dk.brics.automaton.RunAutomaton(auto, true)
    val javaRegexp = java.util.regex.Pattern.compile(re)
    (other, runauto, javaRegexp)
  }

  for (ri <- 0 to regexps.length-1; (lms,dk,jav) = allRegexps(ri)) {
      for (i <- 0 to inputs.length-1; input = inputs(i)) {
        val dkResult = dk.run(input)
        val lmsResult = fullmatch(lms)(input)
        val javResult = jav.matcher(input).matches()

        assert(dkResult == lmsResult)

        val expectedResult = expected(ri)(i)
        assert(dkResult == expectedResult)
        assert(lmsResult == expectedResult)
        assert(javResult == expectedResult)
      }
  }

  println("Testing java.util.regex ...")
  val javaStartTime = System.currentTimeMillis()
  for ((_,_,re) <- allRegexps) {
    for (iter <- 1 to ITERATIONS) {
      for (input <- inputs) {
        val result = re.matcher(input).matches()
      }
    }
  }
  val javaEndTime = System.currentTimeMillis()
  println("... " + (javaEndTime - javaStartTime))

  println("Testing dk.brics.automaton ...")
  val dkStartTime = System.currentTimeMillis()
  for ((_,runauto,_) <- allRegexps) {
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
