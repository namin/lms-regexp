import org.scalatest._

class TestConsistency extends Suite {
  import RegexpType._
  import InputType._

  private def spec(regexp: RegexpType, input: InputType) = {
    (regexp, input) match {
      case (ANY_AAB | ANY_AAB_ANY | COOK, AAB | AABABBAAB) => true
      case (ANY_AAB | ANY_AAB_ANY, X_AAB) => true
      case (ANY_AAB_ANY, AAB_X | AAB_GARBAGE) => true
      case (USD, USD_EX) => true
      case (ANY, _) => true
      case _ => false
    }
  }

  def testAll() = {
    for (matcher <- MatcherType.values) {
      for (regexp <- RegexpType.values) {
	val m = matcher.create(regexp)
	for (input <- InputType.values) {
	  expect(spec(regexp, input), matcher + " for " + regexp + " on " + input) {
	    m.matches(input.text)
	  }
	}
      }
    }
  }
}
