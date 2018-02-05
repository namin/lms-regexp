import org.scalameter.api._
import org.scalameter.picklers.noPickler._

object BenchmarkQuick extends Bench.LocalTime {
  val matchers : Gen[MatcherType] = Gen.enumeration("matcher")(MatcherType.values:_*)

  val inputs = for (inputType <- InputType.values) yield inputType.text

  performance of "matcher" in {
    measure method "matches" in {
      using(matchers) in { m =>
        var count = 0
        val regexps = for (regexpType <- RegexpType.values) yield m.create(regexpType)
        for (regexp <- regexps; input <- inputs) {
          if (regexp.matches(input)) {
            count += 1
          }
        }
        count
      }
    }
  }
}
