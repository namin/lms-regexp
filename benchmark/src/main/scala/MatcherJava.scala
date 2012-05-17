class MatcherJava(regexp: RegexpType) extends RegexpMatcher(regexp) {
  val compiledPattern = java.util.regex.Pattern.compile(
      regexp.re, java.util.regex.Pattern.DOTALL)

  override def matches(input: String): Boolean = {
    compiledPattern.matcher(input).matches
  }
}
