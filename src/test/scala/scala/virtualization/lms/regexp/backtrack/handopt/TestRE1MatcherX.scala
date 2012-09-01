package scala.virtualization.lms.regexp.backtrack.handopt

object TestRE1MatcherX  {

  def run0: Unit = {
    System.out.println("re1matcher0")
    
    val re = """(((\w+):\/\/)([^\/:]*)(:(\d+))?)?([^#?]*)(\?([^#]*))?(#(.*))?"""
    val in = "uggc://jjj.snprobbx.pbz/ybtva.cuc"
    val cp = 0
    val f = new re1matcher0()

    def run() = {
      f(cp, in)
    }
    
    for (i <- 0 until 10) {
      val start = System.currentTimeMillis
      var i = 0
      while (i < 2298*64) {
        run()
        i += 1
      }
      println("elapsed: " + (System.currentTimeMillis - start))
    }
  }  
  

  def run1: Unit = {
    System.out.println("re1matcher1")
    
    val re = """(((\w+):\/\/)([^\/:]*)(:(\d+))?)?([^#?]*)(\?([^#]*))?(#(.*))?"""
    val in = "uggc://jjj.snprobbx.pbz/ybtva.cuc"
    val cp = 0
    val f = new re1matcher1()

    def run() = {
      f(cp, in)
    }
    
    for (i <- 0 until 10) {
      val start = System.currentTimeMillis
      var i = 0
      while (i < 2298*64) {
        run()
        i += 1
      }
      println("elapsed: " + (System.currentTimeMillis - start))
    }
  }  

  def main(args: Array[String]): Unit = {
    run0()
    run1()
  }

}