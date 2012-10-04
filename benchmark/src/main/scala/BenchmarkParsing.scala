import com.google.caliper.Param

class BenchmarkParsing extends SimpleScalaBenchmark {
  val matcher = new PMatcherLMS()
  @Param(Array("10", "100", "1000", "10000", "100000"))
  val length: Int = 0

  var input : String = _
  override def setUp() {
    input = (for (i <- 1 to length) yield "a").mkString("", "", "c")
  }

  def timeMatching(reps: Int) = repeat(reps) {
    matcher.matches(input)
  }
}
