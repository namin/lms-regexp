import org.scalameter.api._
import org.scalameter.picklers.noPickler._

object BenchmarkBig extends Bench.LocalTime {
  val matcherType : Gen[MatcherType] = Gen.enumeration("matcher")(MatcherType.LMS, MatcherType.DK)
  val regexpType : Gen[RegexpType] = Gen.enumeration("regexp")(RegexpType.ANY_AAB, RegexpType.ANY_AAB_ANY, RegexpType.COOK)
  val patternType : Gen[PatternType] = Gen.enumeration("pattern")(PatternType.values:_*)
  val length: Gen[Int] =   Gen.single("length")(10000000)

  def exs : Gen[(RegexpMatcher,String)] =
    for (
      m <- matcherType;
      r <- regexpType;
      p <- patternType;
      l <- length) yield (m.create(r), input(p,l))

  def input(pattern: PatternType, length: Int): String = {
    val sb = new StringBuilder()
    var i = pattern.end.length
    while (i < length) {
      sb.append(pattern.rep)
      i += pattern.rep.length
    }
    sb.append(pattern.end)
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
