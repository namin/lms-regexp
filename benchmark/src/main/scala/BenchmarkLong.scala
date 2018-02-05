import org.scalameter.api._
import org.scalameter.picklers.noPickler._

object BenchmarkLong extends Bench.LocalTime {
  val matcherType : Gen[MatcherType] = Gen.enumeration("matcher")(MatcherType.LMS, MatcherType.DK)
  val regexpType : Gen[RegexpType] = Gen.enumeration("regexp")(RegexpType.ANY_AAB, RegexpType.ANY_AAB_ANY, RegexpType.COOK, RegexpType.USD)
  val length: Gen[Int] = Gen.enumeration("length")(10, 100, 1000, 10000, 100000, 1000000, 10000000)

  def exs : Gen[(RegexpMatcher,String)] =
    for (
      m <- matcherType;
      r <- regexpType;
      l <- length) yield (m.create(r), input(l))

  def input(length: Int): String = {
    val sb = new StringBuilder()
    var i = 1
    while (i < length) {
      sb.append('A')
      i += 1
    }
    sb.append('B')
    sb.toString
  }

  performance of "matcher" in {
    measure method "matches" in {
      using(exs) in { ex =>
        val (regexp,input) = ex
        regexp.matches(input)
      }
    }
  }
}
