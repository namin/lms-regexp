import com.google.caliper.Param

class BenchmarkLong extends SimpleScalaBenchmark {
  @Param(Array("LMS", "DK"))
  val matcherType : MatcherType = null
  @Param(Array("ANY_AAB", "ANY_AAB_ANY", "COOK", "USD"))
  val regexpType : RegexpType = null
  @Param(Array("10", "100", "1000", "10000", "100000", "1000000", "10000000"))
  val length: Int = 0

  var regexp : RegexpMatcher = _
  var input : String = _
  override def setUp() {
    regexp = matcherType.create(regexpType)
    val sb = new StringBuilder()
    var i = 1
    while (i < length) {
      sb.append('A')
      i += 1
    }
    sb.append('B')
    input = sb.toString
  }

  def timeMatching(reps: Int) = repeat(reps) {
    if (regexp.matches(input)) reps else -reps
  }
}
