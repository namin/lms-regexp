import com.google.caliper.Param

class Benchmark extends SimpleScalaBenchmark {
  @Param
  val matcherType : MatcherType = null
  @Param(Array("ANY_AAB", "ANY_AAB_ANY", "USD", "COOK")) // all but ANY
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
    if (regexp.matches(input)) reps else -reps
  }
}
