import org.scalameter.api._
import org.scalameter.picklers.noPickler._

object Benchmark extends Bench.LocalTime {
  val matcherType : Gen[MatcherType] = Gen.enumeration("matcher")(MatcherType.values:_*)
  val regexpType : Gen[RegexpType] = Gen.enumeration("regexp")(RegexpType.values.filter(_!=RegexpType.ANY):_*)
  val inputType : Gen[InputType] = Gen.enumeration("input")(InputType.values:_*)

  def exs : Gen[(RegexpMatcher,String)] =
    for (
      m <- matcherType;
      r <- regexpType;
      i <- inputType) yield (m.create(r), i.text)

  performance of "matcher" in {
    measure method "matches" in {
      using(exs) in { ex =>
        val (regexp,input) = ex
        regexp.matches(input)
      }
    }
  }
}
