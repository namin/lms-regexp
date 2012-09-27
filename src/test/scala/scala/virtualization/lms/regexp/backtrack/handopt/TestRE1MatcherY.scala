package scala.virtualization.lms.regexp.backtrack.handopt

import scala.virtualization.lms.regexp.backtrack._
import java.util.regex.Pattern

object TestRE1MatcherNaive {

  def runNaive: Unit = {
    System.out.println("run naive")
    
    Rhino.debug = false
    
    val re = """(((\w+):\/\/)([^\/:]*)(:(\d+))?)?([^#?]*)(\?([^#]*))?(#(.*))?"""
    val in = "uggc://jjj.snprobbx.pbz/ybtva.cuc"
    
    val rno = RhinoParser.compileREStub(re, "", false)

    val gData = RhinoMatcher.matchNaive(rno, in, 0) // first run will compile
    val f = rno.matcher

    def run = {
      
      //val res1 = RhinoMatcher.matchNaive(rno, in, 0)
      //val str2 = if (res2 == null) "null" else res2 //+ "/" + (res2.groups(in).mkString(","))
      //println(str2)
      //println("done naive: " + res1)
      
      //val res2 = RhinoMatcher.matchStaged(rno, in, 0)
      gData.skipped = 0
      gData.cp = 0
      f()
      
      //val str2 = if (res2 == null) "null" else res2 //+ "/" + (res2.groups(in).mkString(","))
      //println(str2)
      //println("done staged: " + res2)
    }
    
    for (i <- 0 until 10) {
      val start = System.currentTimeMillis
      var i = 0
      while (i < 2298*64) {
        run
        i += 1
      }
      println("elapsed: " + (System.currentTimeMillis - start))
    }
  }  
  def main(args: Array[String]): Unit = {
    runNaive
  }
}

object TestRE1MatcherStaged {
  def runStaged: Unit = {
    System.out.println("run staged")
    
    Rhino.debug = false
    RhinoMatcher.stmatcher.IR.dumpGeneratedCode = true
    
    val re = """(((\w+):\/\/)([^\/:]*)(:(\d+))?)?([^#?]*)(\?([^#]*))?(#(.*))?"""
    val in = "uggc://jjj.snprobbx.pbz/ybtva.cuc"
    
    val rno = RhinoParser.compileREStub(re, "", false)

    val gData = RhinoMatcher.matchStaged(rno, in, 0) // first run will compile
    val f = rno.stmatcher

    def run = {
      
      //val res1 = RhinoMatcher.matchNaive(rno, in, 0)
      //val str2 = if (res2 == null) "null" else res2 //+ "/" + (res2.groups(in).mkString(","))
      //println(str2)
      //println("done naive: " + res1)
      
      //val res2 = RhinoMatcher.matchStaged(rno, in, 0)
      gData.skipped = 0
      gData.cp = 0
      f()
      
      //val str2 = if (res2 == null) "null" else res2 //+ "/" + (res2.groups(in).mkString(","))
      //println(str2)
      //println("done staged: " + res2)
    }
    
    for (i <- 0 until 10) {
      val start = System.currentTimeMillis
      var i = 0
      while (i < 2298*64) {
        run
        i += 1
      }
      System.out.println("elapsed: " + (System.currentTimeMillis - start))
    }
  }  
  
  def main(args: Array[String]): Unit = {
    runStaged
  }
}

object TestRE1MatcherJDK {
  def runJdk: Unit = {
    System.out.println("run jdk")
    
    Rhino.debug = false
    RhinoMatcher.stmatcher.IR.dumpGeneratedCode = true
    
    val re = """(((\w+):\/\/)([^\/:]*)(:(\d+))?)?([^#?]*)(\?([^#]*))?(#(.*))?"""
    val in = "uggc://jjj.snprobbx.pbz/ybtva.cuc"
    
    val pat = Pattern.compile(re)

    val matcher = pat.matcher(in)
    
    def run = {
      matcher.find(0) // find with int arg will reset matcher
    }
    
    for (i <- 0 until 10) {
      val start = System.currentTimeMillis
      var i = 0
      while (i < 2298*64) {
        run
        i += 1
      }
      System.out.println("elapsed: " + (System.currentTimeMillis - start))
    }
  }  
  
  def main(args: Array[String]): Unit = {
    runJdk
  }
}
