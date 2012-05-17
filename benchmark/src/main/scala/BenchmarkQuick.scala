import com.google.caliper.Param

class BenchmarkQuick extends SimpleScalaBenchmark {
  @Param
  val matcherType : MatcherType = null

  var regexps : Array[RegexpMatcher] = _
  var inputs : Array[String] = _
  override def setUp() {
    regexps = for (regexpType <- RegexpType.values) yield matcherType.create(regexpType)
    inputs = for (inputType <- InputType.values) yield inputType.text
  }

  def timeMatching(reps: Int) = repeat(reps) {
    var count = 0
    var i = 0
    val n = regexps.length
    val m = inputs.length
    while (i < n) {
      var j = 0
      while (j < m) {
        if (regexps(i).matches(inputs(j))) count += 1 else count -= 1
        j += 1
      }
      i += 1
    }
    count
  }
}
