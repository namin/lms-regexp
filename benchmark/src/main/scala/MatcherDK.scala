class MatcherDK(regexp: RegexpType) extends RegexpMatcher(regexp) {
  private val auto = new dk.brics.automaton.RunAutomaton(
      new dk.brics.automaton.RegExp(regexp.re).toAutomaton(), true)

  override def matches(input: String): Boolean = {
    auto.run(input)
  }
}
