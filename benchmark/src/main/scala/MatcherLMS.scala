import RegexpType._

class MatcherLMS(regexp: RegexpType) extends RegexpMatcher(regexp) {
  private val fc = regexp match {
    case ANY_AAB => new MatchAAB
    case ANY_AAB_ANY => new MatchAABany
    case USD => new MatchUSD
    case COOK => new MatchCook
    case ANY => new MatchAnything
  }

  override def matches(input: String): Boolean = {
    fc.apply(input)
  }
}
