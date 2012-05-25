import com.google.caliper.Param

class BenchmarkBig extends SimpleScalaBenchmark {
  @Param(Array("LMS", "DK"))
  val matcherType : MatcherType = null
  @Param(Array("ANY_AAB", "ANY_AAB_ANY", "COOK"))
  val regexpType : RegexpType = null
  @Param
  val patternType : PatternType = null
  @Param(Array("10000000"))
  val length: Int = 0

  var regexp : RegexpMatcher = _
  var input : String = _
  override def setUp() {
    regexp = matcherType.create(regexpType)
    val sb = new StringBuilder()
    var i = patternType.end.length;
    while (i < length) {
      sb.append(patternType.rep)
      i += patternType.rep.length;
    }
    sb.append(patternType.end)
    input = sb.toString
  }

  def timeMatching(reps: Int) = repeat(reps) {
    if (regexp.matches(input)) reps else -reps
  }
}
