import com.google.caliper.{Runner => CaliperRunner}

object Runner {

  def main(args: Array[String]) {
    if      (!args.isEmpty && args.head == "quick") CaliperRunner.main(classOf[BenchmarkQuick], args.tail)
    else if (!args.isEmpty && args.head == "long")  CaliperRunner.main(classOf[BenchmarkLong] , args.tail)
    else if (!args.isEmpty && args.head == "big")   CaliperRunner.main(classOf[BenchmarkBig] , args.tail)
    else CaliperRunner.main(classOf[Benchmark], args)
  }
  
}
