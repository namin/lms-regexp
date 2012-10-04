import com.google.caliper.Param

class BenchmarkParsing extends SimpleScalaBenchmark {
  @Param
  val pmatcherType : PMatcherType = null
  // can go up to 100000 with LMS, but not Rhino
  @Param(Array("10000", "1000", "100", "10"))
  val length: Int = 0

  var pmatcher : PMatcher = _
  var input : String = _
  override def setUp() {
    pmatcher = pmatcherType.create()
    input = (for (i <- 1 to length) yield "a").mkString("", "", "c")
  }

  def timeMatching(reps: Int) = repeat(reps) {
    pmatcher.matches(input)
  }
}
