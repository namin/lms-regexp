import com.google.caliper.Param

class Benchmark extends SimpleScalaBenchmark {
  @Param
  val matcherType : MatcherType = null
  @Param
  val regexpType : RegexpType = null
  @Param
  val inputType : InputType = null

  var regexp : RegexpMatcher = _
  var input : String = _
  override def setUp() {
    regexp = matcherType.create(regexpType)
    input = inputType.text
  }

  def timeMatching(reps: Int) = repeat(reps) {
    regexp.matches(input)
  }
}
