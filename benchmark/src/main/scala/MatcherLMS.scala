class MatcherLMS(regexp: RegexpType) extends RegexpMatcher(regexp) {
  private val fc = regexp.toString match {
    case "ANY_AAB" => new MatchAAB
    case "ANY_AAB_ANY" => new MatchAABany
    case "USD" => new MatchUSD
    case "ANY" => new MatchAnything
  }
  private val initialState = fc()

  override def matches(input: String): Boolean = {
    var state = initialState
    var i = 0
    val n = input.length
    while ((state.out & 2) != 2 && (i < n)) {
      state = state.next(input.charAt(i))
      i += 1
    }
    state.out % 2 == 1
  }
}
