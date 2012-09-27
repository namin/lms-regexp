package scala.virtualization.lms.regexp.backtrack

import scala.virtualization.lms.regexp.FileDiffSuite
import org.scalatest._

object Util extends REIntf {

var seen = new scala.collection.mutable.HashSet[String]
var count = 0

var printKey = true
var printRes = false
var printElapsed = false
var dumpCode = false
var runNaive = false
var optUnsafe = false
var disableReplace = false

def reset() = {
  printKey = false
  printRes = false
  printElapsed = false
  dumpCode = false
  runNaive = false
  optUnsafe = false
  count = 0
  cache.clear
}

def log[T](key: String, res: T): T = {
  if (count < 50000 && !(seen contains key)) {
    //seen += key
    count += 1
    if (printKey) println(key)
    if (printRes) println("res " + res)
  }
  res
}

import java.util.regex._


val cache = new scala.collection.mutable.HashMap[String, RECompiled]

def runMatch(rno: RECompiled, s: String, idx: Int) = 
  if (runNaive) RhinoMatcher.matchNaive(rno, s, idx)
  else {
    RhinoMatcher.stmatcher.IR.dumpGeneratedCode = dumpCode
    val res = RhinoMatcher.matchStaged(rno, s, idx)
    RhinoMatcher.stmatcher.IR.dumpGeneratedCode = false
    res
  }


def infix_R(s: String): Regexp = new Regexp(s)

class Regexp(val patternString: String) {
  
  val rno = cache.getOrElseUpdate(patternString, RhinoParser.compileREStub(special(patternString), patternString.substring(patternString.lastIndexOf("/")+1), false))
  
  
  var caseInsensitive = patternString.substring(patternString.lastIndexOf("/")).contains("i")
  var global = patternString.substring(patternString.lastIndexOf("/")).contains("g")
  var lastIndex = 0
  
  def special(s: String) = s match {
    case """/^[\s[]?shapgvba/""" => """^[\s\[]?shapgvba"""
    case _ => 
      s.substring(1, s.lastIndexOf("/"))
  }
  
  def unspecial(s: String) = {
    if (special(patternString) == s) patternString
    else s
  }
    
  override def toString = patternString
}
  

class MatchResult(val index: Int, val input: String, val array: Array[String]) {
  override def toString = array.map(s=>if (s eq null) "" else s).mkString(",")
}


def infix_exec(r: Regexp, s: String): Any = {  
  
  def run(): Any = {
    
    if (r.global && r.lastIndex > s.length()) { 
      r.lastIndex = 0 
      return null
    }

    val gData = runMatch(r.rno, s, r.lastIndex)
    if (gData == null) {
      r.lastIndex = 0
      return null
    }
    
    if (r.global)
      r.lastIndex = gData.cp
    
    val groups = if (optUnsafe) null else gData.groups(s)
    
    //println(gData)
    
    new MatchResult(gData.skipped, s, groups)
  }

  if (!printKey) return run()
  
  val key = "exec " + r + " @ " + s
  log(key, run())
}

def infix_split(s: String, r: Regexp): Any = {
  def run(): Any = {
    val data = s.split(r.special(r.patternString)) // TODO: use Rhino
    new MatchResult(0,s,data)
  }
  
  if (!printKey) return run()
  
  val key = "split " + r + " @ " + s  
  log(key, run())
}

def infix_matches(s: String, r: Regexp): Any = {
  
  // without /g, same as r.exec(s): return array with first match of each group
  // with /g, return all matches of whole expr
  
  def run(): Any = {
    // str.match(regexp)
    // if not global flag, then RegExp.exec, else array with all matches
    if (!r.global) {
        assert(false)
        return r.exec(s) // this will cause an extra line of output: 'exec ...'
    }

    /*// array with all matches
    val m = r.pat.right.get.matcher(s);
    val data = new scala.collection.mutable.ArrayBuffer[String]
    while (m.find()) {
      data += m.group()
    }
    
    if (data.isEmpty) return null
    new MatchResult(0,s,data.toArray) // TODO: use Rhino
    */ return null
  }

  if (!printKey) return run()

  val key = "match " + r + " @ " + s
  log(key, run())
}

def infix_replace(s: String, r: Regexp, s2: String): Any = {

  def run(): Any = {
    
    //if (r.patternString == """/(^|[^\\])\"\\\/Qngr\((-?[0-9]+)\)\\\/\"/g""")
      //return ()//println("abort " + r.patternString)

    if (disableReplace) {     //OPT not actually replacing anything, just doing the matches
      return null
      /*var i = 0
      val end = s.length
      var cnt = 0
      while (i < end) {
        if (s.charAt(i) == '/') cnt += 1
        i += 1
      }
      return cnt*/
    }
    
    
    var idx = 0
    var cnt = 0
    val acc = new java.lang.StringBuilder    
    
    def fix(res: String) = if (res.nonEmpty && res.last == '\n') res.init else res
    
    while (true) {
      val gData = runMatch(r.rno, s, idx)

      if (gData == null) {
        acc append s.substring(idx, s.length)
        return fix(acc.toString)
      }
    
      acc append s.substring(idx, gData.skipped)
      acc append s2
      
      if (!r.global || gData.cp >= s.length) {
        acc append s.substring(gData.cp, s.length)
        return fix(acc.toString)
      }
      
      if (gData.cp == idx) {   // need to make progress (don't check same pos again...)
        acc append s.substring(gData.cp, gData.cp + 1)
        idx = gData.cp + 1
      } else
        idx = gData.cp
      cnt += 1
    }
  }

  if (!printKey) return run()
  
  val key = "replace " + r + " @ " + s + " -- " + s2
  log(key, run())
}

}



object UtilJDK extends REIntf {

import java.util.regex._


def infix_R(s: String): Regexp = new Regexp(s)

class Regexp(val patternString: String) {
  
  var caseInsensitive = patternString.substring(patternString.lastIndexOf("/")).contains("i")
  var global = patternString.substring(patternString.lastIndexOf("/")).contains("g")
  var lastIndex = 0
  
  def special(s: String) = s match {
    case """/^[\s[]?shapgvba/""" => """^[\s\[]?shapgvba"""
    case _ => 
      s.substring(1, s.lastIndexOf("/"))
  }
  
  def unspecial(s: String) = {
    if (special(patternString) == s) patternString
    else s
  }
  
  // cache? no, Pattern.compile should worry about it
  lazy val pat = try { 
    if (caseInsensitive) Right(Pattern.compile(special(patternString), Pattern.CASE_INSENSITIVE))
    else Right(Pattern.compile(special(patternString))) 
  } catch { case ex => Left(ex) }
  
  override def toString = pat match { case Right(p) => unspecial(p.toString) case Left(p) => p.toString }
    //patternString + " " + pat
}

class MatchResult(val index: Int, val input: String, val array: Array[String]) {
  override def toString = array.map(s=>if (s eq null) "" else s).mkString(",")
}


def infix_exec(r: Regexp, s: String): Any = {  
  
  def run(): Any = {
    // return an array with the first match of each group (/g does not return multiple matches)
    // starts at lastIndex!!!! if /g
  
    // if global flag, exec starts from 'lastIndex'
    // check str is long enough if we are going from 'lastIndex'
 
    if (r.global && r.lastIndex > s.length()) { 
      r.lastIndex = 0 
      return null
    }
 
    val m = r.pat.right.get.matcher(s) 
    if (!m.find(r.lastIndex)) {
      r.lastIndex = 0
      return null
    }
 
    // update 'lastIndex' if global
    if (r.global) {
      r.lastIndex = m.end();
    }
 
 
    val groupCount = 1 + r.patternString.replace("(?"/*)*/, "").count(_ == '('/*)*/)
 
    // put results into array
    val data = new Array[String](groupCount) // m.groupCount may be off ... (?)
    
//    if (data.length != m.groupCount)
//      println("pad: " + r + " @ " + s)
 
    for (i <- 0 until data.length) {
      data(i) = try { m.group(i) } catch { case _ => null: String }
    }
 
    // put 'index' and 'input' 
    new MatchResult(m.start(), s, data)
  }
  
  
  if (!Util.printKey) return run()

  val key = "exec " + r + " @ " + s
  Util.log(key, run())
}

def infix_split(s: String, r: Regexp): Any = {
  def run(): Any = {
    val data = s.split(r.special(r.patternString))
    new MatchResult(0,s,data)
  }
  
  if (!Util.printKey) return run()
  
  val key = "split " + r + " @ " + s
  Util.log(key, run())
}

def infix_matches(s: String, r: Regexp): Any = {
  
  // without /g, same as r.exec(s): return array with first match of each group
  // with /g, return all matches of whole expr
  
  def run(): Any = {
    // str.match(regexp)
    // if not global flag, then RegExp.exec, else array with all matches
    if (!r.global) {
        assert(false)
        return r.exec(s) // this will cause an extra line of output: 'exec ...'
    }

    // array with all matches
    val m = r.pat.right.get.matcher(s);
    val data = new scala.collection.mutable.ArrayBuffer[String]
    while (m.find()) {
      data += m.group()
    }
    
    if (data.isEmpty) return null
    new MatchResult(0,s,data.toArray)
  }

  if (!Util.printKey) return run()

  val key = "match " + r + " @ " + s
  Util.log(key, run())
}

def infix_replace(s: String, r: Regexp, s2: String): Any = {

  def run(): Any = {
    
    if (Util.disableReplace)
      return null

    val m = r.pat.right.get.matcher(s);
    
    val res = if (!r.global) m.replaceFirst(s2) else m.replaceAll(s2)
    if (res.nonEmpty && res.last == '\n') res.init else res
  }

  if (!Util.printKey) return run()
  
  val key = "replace " + r + " @ " + s + " -- " + s2
  Util.log(key, run())
}

}




object TestV8BenchAllNaive extends V8Bench {

  val REImpl = Util
  
  def main(args: Array[String]): Unit = {
    System.out.println("HELLO")
    Util.reset()
    Util.runNaive = true
    Util.printElapsed = true
    
    for (i <- 0 until 10)
      run(64)
  }  
  
}

object TestV8BenchAllStaged extends V8Bench {

  val REImpl = Util
  
  def main(args: Array[String]): Unit = {
    System.out.println("HELLO")
    Util.reset()
    Util.printElapsed = true
    Util.dumpCode = true

    for (i <- 0 until 10)
      run(64)
  }  
  
}


object TestV8BenchAllStagedUnsafe extends V8Bench {

  val REImpl = Util
  
  def main(args: Array[String]): Unit = {
    System.out.println("HELLO")
    Util.reset()
    Util.printElapsed = true
    Util.dumpCode = true
    Util.optUnsafe = true

    for (i <- 0 until 10)
      run(64)
  }  
  
}



object TestV8BenchAllJDK extends V8Bench {

  val REImpl = UtilJDK
  
  def main(args: Array[String]): Unit = {
    System.out.println("HELLO")
    Util.reset()
    Util.printElapsed = true
    
    for (i <- 0 until 10)
      run(64)
  }  
  
}

object TestV8BenchNoReplaceNaive extends V8Bench {

  val REImpl = Util
  
  def main(args: Array[String]): Unit = {
    System.out.println("HELLO")
    Util.reset()
    Util.printElapsed = true
    Util.runNaive = true
    Util.disableReplace = true
    
    for (i <- 0 until 10)
      run(64)
  }  
  
}

object TestV8BenchNoReplaceStaged extends V8Bench {

  val REImpl = Util
  
  def main(args: Array[String]): Unit = {
    System.out.println("HELLO")
    Util.reset()
    Util.printElapsed = true
    Util.dumpCode = true
    Util.disableReplace = true
    
    for (i <- 0 until 10)
      run(64)
  }  
  
}

object TestV8BenchNoReplaceStagedUnsafe extends V8Bench {

  val REImpl = Util
  
  def main(args: Array[String]): Unit = {
    System.out.println("HELLO")
    Util.reset()
    Util.printElapsed = true
    Util.dumpCode = true
    Util.disableReplace = true
    Util.optUnsafe = true

    for (i <- 0 until 10)
      run(64)
  }  
  
}



object TestV8BenchNoReplaceJDK extends V8Bench {

  val REImpl = UtilJDK
  
  def main(args: Array[String]): Unit = {
    System.out.println("HELLO")
    Util.reset()
    Util.printElapsed = true
    Util.disableReplace = true
    
    for (i <- 0 until 10)
      run(64)
  }  
  
}






class TestV8 extends V8Bench with FileDiffSuite {
  
  override val REImpl = Util
  
  def testV80 = withOutFileChecked("test-out/v8regexp0") {
    // output doesn't match check file 100% -- non printing chars?
    Util.reset()
    Util.printKey = true
    Util.printRes = false
    Util.runNaive = true
    runSuite(1)
    println("done: " + Util.count)
  }

  def testV81 = withOutFileChecked("test-out/v8regexp1") {
    Util.reset()
    Util.printKey = true
    Util.printRes = true
    Util.runNaive = true
    runSuite(1)
    println("done: " + Util.count)
  }

  def testV82 = withOutFileChecked("test-out/v8regexp2") {
    Util.reset()
    Util.printKey = true
    Util.printRes = true
    Util.runNaive = false
    runSuite(1)
    println("done: " + Util.count)
  }

}


abstract class REIntf {
  
  type Regexp
  
  def infix_R(s: String): Regexp

  def infix_exec(r: Regexp, s: String): Any

  def infix_split(s: String, r: Regexp): Any 
  def infix_matches(s: String, r: Regexp): Any
  def infix_replace(s: String, r: Regexp, s2: String): Any

}



abstract class V8Bench {

  val REImpl: REIntf
  import REImpl._

  def run(benchIter: Int) = {
    val start = System.currentTimeMillis
    runSuite(benchIter)
    val elapsed = System.currentTimeMillis - start
    if (Util.printElapsed) System.out.println("elapsed: " + elapsed)
  }

  

  def runSuite(benchIter: Int) = {

    //---base.js---
    // Copyright 2008 the V8 project authors. All rights reserved.
    // Redistribution and use in source and binary forms, with or without
    // modification, are permitted provided that the following conditions are
    // met:
    //
    //     * Redistributions of source code must retain the above copyright
    //       notice, this list of conditions and the following disclaimer.
    //     * Redistributions in binary form must reproduce the above
    //       copyright notice, this list of conditions and the following
    //       disclaimer in the documentation and/or other materials provided
    //       with the distribution.
    //     * Neither the name of Google Inc. nor the names of its
    //       contributors may be used to endorse or promote products derived
    //       from this software without specific prior written permission.
    //
    // THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    // "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    // LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    // A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    // OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    // SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    // LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    // DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    // THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    // (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    // OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    // To make the benchmark results predictable, we replace Math.random
    // with a 100% deterministic alternative.
    object MathD {
      var seed = 49734321;
      def random(): Double = {
        // Robert Jenkins' 32 bit integer hash function.
        seed = ((seed + 0x7ed55d16) + (seed << 12))  & 0xffffffff;
        seed = ((seed ^ 0xc761c23c) ^ (seed >>> 19)) & 0xffffffff;
        seed = ((seed + 0x165667b1) + (seed << 5))   & 0xffffffff;
        seed = ((seed + 0xd3a2646c) ^ (seed << 9))   & 0xffffffff;
        seed = ((seed + 0xfd7046c5) + (seed << 3))   & 0xffffffff;
        seed = ((seed ^ 0xb55a4f09) ^ (seed >>> 16)) & 0xffffffff;
        return (seed & 0xfffffff) / 0x10000000.toDouble;
      };
    }

    //---regexp.js---
    // Copyright 2010 the V8 project authors. All rights reserved.
    // Redistribution and use in source and binary forms, with or without
    // modification, are permitted provided that the following conditions are
    // met:
    //
    //     * Redistributions of source code must retain the above copyright
    //       notice, this list of conditions and the following disclaimer.
    //     * Redistributions in binary form must reproduce the above
    //       copyright notice, this list of conditions and the following
    //       disclaimer in the documentation and/or other materials provided
    //       with the distribution.
    //     * Neither the name of Google Inc. nor the names of its
    //       contributors may be used to endorse or promote products derived
    //       from this software without specific prior written permission.
    //
    // THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
    // "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
    // LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
    // A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
    // OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
    // SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
    // LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
    // DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
    // THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
    // (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
    // OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

    // Automatically generated on 2009-01-30. Manually updated on 2010-09-17.

    // This benchmark is generated by loading 50 of the most popular pages
    // on the web and logging all regexp operations performed.  Each
    // operation is given a weight that is calculated from an estimate of
    // the popularity of the pages where it occurs and the number of times
    // it is executed while loading each page.  Furthermore the literal
    // letters in the data are encoded using ROT13 in a way that does not
    // affect how the regexps match their input.  Finally the strings are
    // scrambled to exercise the regexp engine on different input strings.


    // Returns an array of n different variants of the input string str.
    // The variants are computed by randomly rotating one random
    // character.
    def computeInputVariants(str: String, n: Int) = {
      var variants = new Array[String](n)
      variants(0) = str
      for (i <- 1 until n) {
        //variants(i) = str
        var pos = (MathD.random() * str.length).toInt;
        var chr = ((str.charAt(pos) + (MathD.random() * 128).toInt) % 128).toChar;
        if (chr < 32.toChar || chr > 126.toChar)
          chr = ' '
        variants(i) = str.substring(0, pos) + chr + str.substring(pos + 1, str.length);
        //assert(variants(i) != variants(i-1), variants(i-1) + " / " + pos + " / " + chr)
      }
      variants;
    }

    def RegExpBenchmark() {
      var re0 = """/^ba/""".R;
      var re1 = """/(((\w+):\/\/)([^\/:]*)(:(\d+))?)?([^#?]*)(\?([^#]*))?(#(.*))?/""".R;
      var re2 = """/^\s*|\s*$/g""".R;
      var re3 = """/\bQBZPbageby_cynprubyqre\b/""".R;
      var re4 = """/,/""".R;
      var re5 = """/\bQBZPbageby_cynprubyqre\b/g""".R;
      var re6 = """/^[\s\xa0]+|[\s\xa0]+$/g""".R;
      var re7 = """/(\d*)(\D*)/g""".R;
      var re8 = """/=/""".R;
      var re9 = """/(^|\s)lhv\-h(\s|$)/""".R;
      var str0 = "Zbmvyyn/5.0 (Jvaqbjf; H; Jvaqbjf AG 5.1; ra-HF) NccyrJroXvg/528.9 (XUGZY, yvxr Trpxb) Puebzr/2.0.157.0 Fnsnev/528.9";
      var re10 = """/\#/g""".R;
      var re11 = """/\./g""".R;
      var re12 = """/'/g""".R;
      var re13 = """/\?[\w\W]*(sevraqvq|punaaryvq|tebhcvq)=([^\&\?#]*)/i""".R;
      var str1 = "Fubpxjnir Synfu 9.0  e115";
      var re14 = """/\s+/g""".R;
      var re15 = """/^\s*(\S*(\s+\S+)*)\s*$/""".R;
      var re16 = """/(-[a-z])/i""".R;

      var s0 = computeInputVariants("pyvpx", 6511);
      var s1 = computeInputVariants("uggc://jjj.snprobbx.pbz/ybtva.cuc", 1844);
      var s2 = computeInputVariants("QBZPbageby_cynprubyqre", 739);
      var s3 = computeInputVariants("uggc://jjj.snprobbx.pbz/", 598);
      var s4 = computeInputVariants("uggc://jjj.snprobbx.pbz/fepu.cuc", 454);
      var s5 = computeInputVariants("qqqq, ZZZ q, llll", 352);
      var s6 = computeInputVariants("vachggrkg QBZPbageby_cynprubyqre", 312);
      var s7 = computeInputVariants("/ZlFcnprUbzrcntr/Vaqrk-FvgrUbzr,10000000", 282);
      var s8 = computeInputVariants("vachggrkg", 177);
      var s9 = computeInputVariants("528.9", 170);
      var s10 = computeInputVariants("528", 170);
      var s11 = computeInputVariants("VCPhygher=ra-HF", 156);
      var s12 = computeInputVariants("CersreerqPhygher=ra-HF", 156);
      var s13 = computeInputVariants("xrlcerff", 144);
      var s14 = computeInputVariants("521", 139);
      var s15 = computeInputVariants(str0, 139);
      var s16 = computeInputVariants("qvi .so_zrah", 137);
      var s17 = computeInputVariants("qvi.so_zrah", 137);
      var s18 = computeInputVariants("uvqqra_ryrz", 117);
      var s19 = computeInputVariants("sevraqfgre_naba=nvq%3Qn6ss9p85n868ro9s059pn854735956o3%26ers%3Q%26df%3Q%26vpgl%3QHF", 95);
      var s20 = computeInputVariants("uggc://ubzr.zlfcnpr.pbz/vaqrk.psz", 93);
      var s21 = computeInputVariants(str1, 92);
      var s22 = computeInputVariants("svefg", 85);
      var s23 = computeInputVariants("uggc://cebsvyr.zlfcnpr.pbz/vaqrk.psz", 85);
      var s24 = computeInputVariants("ynfg", 85);
      var s25 = computeInputVariants("qvfcynl", 85);

      def runBlock0() {
        for (i <- 0 until 6511) {
          re0.exec(s0(i));
        }
        for (i <- 0 until 1844) {
          re1.exec(s1(i));
        }
        for (i <- 0 until 739) {
          s2(i).replace(re2, "");
        }
        for (i <- 0 until 598) {
          re1.exec(s3(i));
        }
        for (i <- 0 until 454) {
          re1.exec(s4(i));
        }
        for (i <- 0 until 352) {
          """/qqqq|qqq|qq|q|ZZZZ|ZZZ|ZZ|Z|llll|ll|l|uu|u|UU|U|zz|z|ff|f|gg|g|sss|ss|s|mmm|mm|m/g""".R.exec(s5(i));
        }
        for (i <- 0 until 312) {
          re3.exec(s6(i));
        }
        for (i <- 0 until 282) {
          re4.exec(s7(i));
        }
        for (i <- 0 until 177) {
          s8(i).replace(re5, "");
        }
        for (i <- 0 until 170) {
          s9(i).replace(re6, "");
          re7.exec(s10(i));
        }
        for (i <- 0 until 156) {
          re8.exec(s11(i));
          re8.exec(s12(i));
        }
        for (i <- 0 until 144) {
          re0.exec(s13(i));
        }
        for (i <- 0 until 139) {
          s14(i).replace(re6, "");
          re7.exec(s14(i));
          re9.exec("");
          """/JroXvg\/(\S+)/""".R.exec(s15(i));
        }
        for (i <- 0 until 137) {
          s16(i).replace(re10, "");
          s16(i).replace("""/\[/g""".R, "");
          s17(i).replace(re11, "");
        }
        for (i <- 0 until 117) {
          s18(i).replace(re2, "");
        }
        for (i <- 0 until 95) {
          """/(?:^|;)\s*sevraqfgre_ynat=([^;]*)/""".R.exec(s19(i));
        }
        for (i <- 0 until 93) {
          s20(i).replace(re12, "");
          re13.exec(s20(i));
        }
        for (i <- 0 until 92) {
          s21(i).replace("""/([a-zA-Z]|\s)+/""".R, "");
        }
        for (i <- 0 until 85) {
          s22(i).replace(re14, "");
          s22(i).replace(re15, "");
          s23(i).replace(re12, "");
          s24(i).replace(re14, "");
          s24(i).replace(re15, "");
          re16.exec(s25(i));
          re13.exec(s23(i));
        }
      }
      var re17 = """/(^|[^\\])\"\\\/Qngr\((-?[0-9]+)\)\\\/\"/g""".R;
/*ESC*/      var str2 = """{"anzr":"","ahzoreSbezng":{"PheeraplQrpvznyQvtvgf":2,"PheeraplQrpvznyFrcnengbe":".","VfErnqBayl":gehr,"PheeraplTebhcFvmrf":[3],"AhzoreTebhcFvmrf":[3],"CrepragTebhcFvmrf":[3],"PheeraplTebhcFrcnengbe":",","PheeraplFlzoby":"?","AnAFlzoby":"AnA","PheeraplArtngvirCnggrea":0,"AhzoreArtngvirCnggrea":1,"CrepragCbfvgvirCnggrea":0,"CrepragArtngvirCnggrea":0,"ArtngvirVasvavglFlzoby":"-Vasvavgl","ArtngvirFvta":"-","AhzoreQrpvznyQvtvgf":2,"AhzoreQrpvznyFrcnengbe":".","AhzoreTebhcFrcnengbe":",","PheeraplCbfvgvirCnggrea":0,"CbfvgvirVasvavglFlzoby":"Vasvavgl","CbfvgvirFvta":"+","CrepragQrpvznyQvtvgf":2,"CrepragQrpvznyFrcnengbe":".","CrepragTebhcFrcnengbe":",","CrepragFlzoby":"%","CreZvyyrFlzoby":"?","AngvirQvtvgf":["0","1","2","3","4","5","6","7","8","9"],"QvtvgFhofgvghgvba":1},"qngrGvzrSbezng":{"NZQrfvtangbe":"NZ","Pnyraqne":{"ZvaFhccbegrqQngrGvzr":"@-62135568000000@","ZnkFhccbegrqQngrGvzr":"@253402300799999@","NytbevguzGlcr":1,"PnyraqneGlcr":1,"Renf":[1],"GjbQvtvgLrneZnk":2029,"VfErnqBayl":gehr},"QngrFrcnengbe":"/","SvefgQnlBsJrrx":0,"PnyraqneJrrxEhyr":0,"ShyyQngrGvzrCnggrea":"qqqq, qq ZZZZ llll UU:zz:ff","YbatQngrCnggrea":"qqqq, qq ZZZZ llll","YbatGvzrCnggrea":"UU:zz:ff","ZbaguQnlCnggrea":"ZZZZ qq","CZQrfvtangbe":"CZ","ESP1123Cnggrea":"qqq, qq ZZZ llll UU':'zz':'ff 'TZG'","FubegQngrCnggrea":"ZZ/qq/llll","FubegGvzrCnggrea":"UU:zz","FbegnoyrQngrGvzrCnggrea":"llll'-'ZZ'-'qq'G'UU':'zz':'ff","GvzrFrcnengbe":":","HavirefnyFbegnoyrQngrGvzrCnggrea":"llll'-'ZZ'-'qq UU':'zz':'ff'M'","LrneZbaguCnggrea":"llll ZZZZ","NooerivngrqQnlAnzrf":["Fha","Zba","Ghr","Jrq","Guh","Sev","Fng"],"FubegrfgQnlAnzrf":["Fh","Zb","Gh","Jr","Gu","Se","Fn"],"QnlAnzrf":["Fhaqnl","Zbaqnl","Ghrfqnl","Jrqarfqnl","Guhefqnl","Sevqnl","Fngheqnl"],"NooerivngrqZbaguAnzrf":["Wna","Sro","Zne","Nce","Znl","Wha","Why","Nht","Frc","Bpg","Abi","Qrp",""],"ZbaguAnzrf":["Wnahnel","Sroehnel","Znepu","Ncevy","Znl","Whar","Whyl","Nhthfg","Frcgrzore","Bpgbore","Abirzore","Qrprzore",""],"VfErnqBayl":gehr,"AngvirPnyraqneAnzr":"Tertbevna Pnyraqne","NooerivngrqZbaguTravgvirAnzrf":["Wna","Sro","Zne","Nce","Znl","Wha","Why","Nht","Frc","Bpg","Abi","Qrp",""],"ZbaguTravgvirAnzrf":["Wnahnel","Sroehnel","Znepu","Ncevy","Znl","Whar","Whyl","Nhthfg","Frcgrzore","Bpgbore","Abirzore","Qrprzore",""]}}""";
/*ESC*/      var str3 = """{"anzr":"ra-HF","ahzoreSbezng":{"PheeraplQrpvznyQvtvgf":2,"PheeraplQrpvznyFrcnengbe":".","VfErnqBayl":snyfr,"PheeraplTebhcFvmrf":[3],"AhzoreTebhcFvmrf":[3],"CrepragTebhcFvmrf":[3],"PheeraplTebhcFrcnengbe":",","PheeraplFlzoby":"$","AnAFlzoby":"AnA","PheeraplArtngvirCnggrea":0,"AhzoreArtngvirCnggrea":1,"CrepragCbfvgvirCnggrea":0,"CrepragArtngvirCnggrea":0,"ArtngvirVasvavglFlzoby":"-Vasvavgl","ArtngvirFvta":"-","AhzoreQrpvznyQvtvgf":2,"AhzoreQrpvznyFrcnengbe":".","AhzoreTebhcFrcnengbe":",","PheeraplCbfvgvirCnggrea":0,"CbfvgvirVasvavglFlzoby":"Vasvavgl","CbfvgvirFvta":"+","CrepragQrpvznyQvtvgf":2,"CrepragQrpvznyFrcnengbe":".","CrepragTebhcFrcnengbe":",","CrepragFlzoby":"%","CreZvyyrFlzoby":"?","AngvirQvtvgf":["0","1","2","3","4","5","6","7","8","9"],"QvtvgFhofgvghgvba":1},"qngrGvzrSbezng":{"NZQrfvtangbe":"NZ","Pnyraqne":{"ZvaFhccbegrqQngrGvzr":"@-62135568000000@","ZnkFhccbegrqQngrGvzr":"@253402300799999@","NytbevguzGlcr":1,"PnyraqneGlcr":1,"Renf":[1],"GjbQvtvgLrneZnk":2029,"VfErnqBayl":snyfr},"QngrFrcnengbe":"/","SvefgQnlBsJrrx":0,"PnyraqneJrrxEhyr":0,"ShyyQngrGvzrCnggrea":"qqqq, ZZZZ qq, llll u:zz:ff gg","YbatQngrCnggrea":"qqqq, ZZZZ qq, llll","YbatGvzrCnggrea":"u:zz:ff gg","ZbaguQnlCnggrea":"ZZZZ qq","CZQrfvtangbe":"CZ","ESP1123Cnggrea":"qqq, qq ZZZ llll UU':'zz':'ff 'TZG'","FubegQngrCnggrea":"Z/q/llll","FubegGvzrCnggrea":"u:zz gg","FbegnoyrQngrGvzrCnggrea":"llll'-'ZZ'-'qq'G'UU':'zz':'ff","GvzrFrcnengbe":":","HavirefnyFbegnoyrQngrGvzrCnggrea":"llll'-'ZZ'-'qq UU':'zz':'ff'M'","LrneZbaguCnggrea":"ZZZZ, llll","NooerivngrqQnlAnzrf":["Fha","Zba","Ghr","Jrq","Guh","Sev","Fng"],"FubegrfgQnlAnzrf":["Fh","Zb","Gh","Jr","Gu","Se","Fn"],"QnlAnzrf":["Fhaqnl","Zbaqnl","Ghrfqnl","Jrqarfqnl","Guhefqnl","Sevqnl","Fngheqnl"],"NooerivngrqZbaguAnzrf":["Wna","Sro","Zne","Nce","Znl","Wha","Why","Nht","Frc","Bpg","Abi","Qrp",""],"ZbaguAnzrf":["Wnahnel","Sroehnel","Znepu","Ncevy","Znl","Whar","Whyl","Nhthfg","Frcgrzore","Bpgbore","Abirzore","Qrprzore",""],"VfErnqBayl":snyfr,"AngvirPnyraqneAnzr":"Tertbevna Pnyraqne","NooerivngrqZbaguTravgvirAnzrf":["Wna","Sro","Zne","Nce","Znl","Wha","Why","Nht","Frc","Bpg","Abi","Qrp",""],"ZbaguTravgvirAnzrf":["Wnahnel","Sroehnel","Znepu","Ncevy","Znl","Whar","Whyl","Nhthfg","Frcgrzore","Bpgbore","Abirzore","Qrprzore",""]}}""";
      var str4 = "HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str5 = "HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var re18 = """/^\s+|\s+$/g""".R;
      var str6 = "uggc://jjj.snprobbx.pbz/vaqrk.cuc";
      var re19 = """/(?:^|\s+)ba(?:\s+|$)/""".R;
      var re20 = """/[+, ]/""".R;
      var re21 = """/ybnqrq|pbzcyrgr/""".R;
/*ESC*/      var str7 = """;;jvaqbj.IjPurpxZbhfrCbfvgvbaNQ_VQ=shapgvba(r){vs(!r)ine r=jvaqbj.rirag;ine c=-1;vs(d1)c=d1.EbyybssCnary;ine bo=IjTrgBow("IjCnayNQ_VQ_"+c);vs(bo&&bo.fglyr.ivfvovyvgl=="ivfvoyr"){ine fns=IjFns?8:0;ine pheK=r.pyvragK+IjBOFpe("U")+fns,pheL=r.pyvragL+IjBOFpe("I")+fns;ine y=IjBOEC(NQ_VQ,bo,"Y"),g=IjBOEC(NQ_VQ,bo,"G");ine e=y+d1.Cnaryf[c].Jvqgu,o=g+d1.Cnaryf[c].Urvtug;vs((pheK<y)||(pheK>e)||(pheL<g)||(pheL>o)){vs(jvaqbj.IjBaEbyybssNQ_VQ)IjBaEbyybssNQ_VQ(c);ryfr IjPybfrNq(NQ_VQ,c,gehr,"");}ryfr erghea;}IjPnapryZbhfrYvfgrareNQ_VQ();};;jvaqbj.IjFrgEbyybssCnaryNQ_VQ=shapgvba(c){ine z="zbhfrzbir",q=qbphzrag,s=IjPurpxZbhfrCbfvgvbaNQ_VQ;c=IjTc(NQ_VQ,c);vs(d1&&d1.EbyybssCnary>-1)IjPnapryZbhfrYvfgrareNQ_VQ();vs(d1)d1.EbyybssCnary=c;gel{vs(q.nqqRiragYvfgrare)q.nqqRiragYvfgrare(z,s,snyfr);ryfr vs(q.nggnpuRirag)q.nggnpuRirag("ba"+z,s);}pngpu(r){}};;jvaqbj.IjPnapryZbhfrYvfgrareNQ_VQ=shapgvba(){ine z="zbhfrzbir",q=qbphzrag,s=IjPurpxZbhfrCbfvgvbaNQ_VQ;vs(d1)d1.EbyybssCnary=-1;gel{vs(q.erzbirRiragYvfgrare)q.erzbirRiragYvfgrare(z,s,snyfr);ryfr vs(q.qrgnpuRirag)q.qrgnpuRirag("ba"+z,s);}pngpu(r){}};;d1.IjTc=d2(n,c){ine nq=d1;vs(vfAnA(c)){sbe(ine v=0;v<nq.Cnaryf.yratgu;v++)vs(nq.Cnaryf[v].Anzr==c)erghea v;erghea 0;}erghea c;};;d1.IjTpy=d2(n,c,p){ine cn=d1.Cnaryf[IjTc(n,c)];vs(!cn)erghea 0;vs(vfAnA(p)){sbe(ine v=0;v<cn.Pyvpxguehf.yratgu;v++)vs(cn.Pyvpxguehf[v].Anzr==p)erghea v;erghea 0;}erghea p;};;d1.IjGenpr=d2(n,f){gel{vs(jvaqbj["Ij"+"QtQ"])jvaqbj["Ij"+"QtQ"](n,1,f);}pngpu(r){}};;d1.IjYvzvg1=d2(n,f){ine nq=d1,vh=f.fcyvg("/");sbe(ine v=0,p=0;v<vh.yratgu;v++){vs(vh[v].yratgu>0){vs(nq.FzV.yratgu>0)nq.FzV+="/";nq.FzV+=vh[v];nq.FtZ[nq.FtZ.yratgu]=snyfr;}}};;d1.IjYvzvg0=d2(n,f){ine nq=d1,vh=f.fcyvg("/");sbe(ine v=0;v<vh.yratgu;v++){vs(vh[v].yratgu>0){vs(nq.OvC.yratgu>0)nq.OvC+="/";nq.OvC+=vh[v];}}};;d1.IjRVST=d2(n,c){jvaqbj["IjCnayNQ_VQ_"+c+"_Bow"]=IjTrgBow("IjCnayNQ_VQ_"+c+"_Bow");vs(jvaqbj["IjCnayNQ_VQ_"+c+"_Bow"]==ahyy)frgGvzrbhg("IjRVST(NQ_VQ,"+c+")",d1.rvsg);};;d1.IjNavzSHC=d2(n,c){ine nq=d1;vs(c>nq.Cnaryf.yratgu)erghea;ine cna=nq.Cnaryf[c],nn=gehr,on=gehr,yn=gehr,en=gehr,cn=nq.Cnaryf[0],sf=nq.ShF,j=cn.Jvqgu,u=cn.Urvtug;vs(j=="100%"){j=sf;en=snyfr;yn=snyfr;}vs(u=="100%"){u=sf;nn=snyfr;on=snyfr;}vs(cn.YnY=="Y")yn=snyfr;vs(cn.YnY=="E")en=snyfr;vs(cn.GnY=="G")nn=snyfr;vs(cn.GnY=="O")on=snyfr;ine k=0,l=0;fjvgpu(nq.NshP%8){pnfr 0:oernx;pnfr 1:vs(nn)l=-sf;oernx;pnfr 2:k=j-sf;oernx;pnfr 3:vs(en)k=j;oernx;pnfr 4:k=j-sf;l=u-sf;oernx;pnfr 5:k=j-sf;vs(on)l=u;oernx;pnfr 6:l=u-sf;oernx;pnfr 7:vs(yn)k=-sf;l=u-sf;oernx;}vs(nq.NshP++ <nq.NshG)frgGvzrbhg(("IjNavzSHC(NQ_VQ,"+c+")"),nq.NshC);ryfr{k=-1000;l=k;}cna.YrsgBssfrg=k;cna.GbcBssfrg=l;IjNhErcb(n,c);};;d1.IjTrgErnyCbfvgvba=d2(n,b,j){erghea IjBOEC.nccyl(guvf,nethzragf);};;d1.IjPnapryGvzrbhg=d2(n,c){c=IjTc(n,c);ine cay=d1.Cnaryf[c];vs(cay&&cay.UgU!=""){pyrneGvzrbhg(cay.UgU);}};;d1.IjPnapryNyyGvzrbhgf=d2(n){vs(d1.YbpxGvzrbhgPunatrf)erghea;sbe(ine c=0;c<d1.bac;c++)IjPnapryGvzrbhg(n,c);};;d1.IjFgnegGvzrbhg=d2(n,c,bG){c=IjTc(n,c);ine cay=d1.Cnaryf[c];vs(cay&&((cay.UvqrGvzrbhgInyhr>0)||(nethzragf.yratgu==3&&bG>0))){pyrneGvzrbhg(cay.UgU);cay.UgU=frgGvzrbhg(cay.UvqrNpgvba,(nethzragf.yratgu==3?bG:cay.UvqrGvzrbhgInyhr));}};;d1.IjErfrgGvzrbhg=d2(n,c,bG){c=IjTc(n,c);IjPnapryGvzrbhg(n,c);riny("IjFgnegGvzrbhg(NQ_VQ,c"+(nethzragf.yratgu==3?",bG":"")+")");};;d1.IjErfrgNyyGvzrbhgf=d2(n){sbe(ine c=0;c<d1.bac;c++)IjErfrgGvzrbhg(n,c);};;d1.IjQrgnpure=d2(n,rig,sap){gel{vs(IjQVR5)riny("jvaqbj.qrgnpuRirag('ba"+rig+"',"+sap+"NQ_VQ)");ryfr vs(!IjQVRZnp)riny("jvaqbj.erzbirRiragYvfgrare('"+rig+"',"+sap+"NQ_VQ,snyfr)");}pngpu(r){}};;d1.IjPyrnaHc=d2(n){IjCvat(n,"G");ine nq=d1;sbe(ine v=0;v<nq.Cnaryf.yratgu;v++){IjUvqrCnary(n,v,gehr);}gel{IjTrgBow(nq.gya).vaareUGZY="";}pngpu(r){}vs(nq.gya!=nq.gya2)gel{IjTrgBow(nq.gya2).vaareUGZY="";}pngpu(r){}gel{d1=ahyy;}pngpu(r){}gel{IjQrgnpure(n,"haybnq","IjHayNQ_VQ");}pngpu(r){}gel{jvaqbj.IjHayNQ_VQ=ahyy;}pngpu(r){}gel{IjQrgnpure(n,"fpebyy","IjFeNQ_VQ");}pngpu(r){}gel{jvaqbj.IjFeNQ_VQ=ahyy;}pngpu(r){}gel{IjQrgnpure(n,"erfvmr","IjEmNQ_VQ");}pngpu(r){}gel{jvaqbj.IjEmNQ_VQ=ahyy;}pngpu(r){}gel{IjQrgnpure(n""";
/*ESC*/      var str8 = """;;jvaqbj.IjPurpxZbhfrCbfvgvbaNQ_VQ=shapgvba(r){vs(!r)ine r=jvaqbj.rirag;ine c=-1;vs(jvaqbj.IjNqNQ_VQ)c=jvaqbj.IjNqNQ_VQ.EbyybssCnary;ine bo=IjTrgBow("IjCnayNQ_VQ_"+c);vs(bo&&bo.fglyr.ivfvovyvgl=="ivfvoyr"){ine fns=IjFns?8:0;ine pheK=r.pyvragK+IjBOFpe("U")+fns,pheL=r.pyvragL+IjBOFpe("I")+fns;ine y=IjBOEC(NQ_VQ,bo,"Y"),g=IjBOEC(NQ_VQ,bo,"G");ine e=y+jvaqbj.IjNqNQ_VQ.Cnaryf[c].Jvqgu,o=g+jvaqbj.IjNqNQ_VQ.Cnaryf[c].Urvtug;vs((pheK<y)||(pheK>e)||(pheL<g)||(pheL>o)){vs(jvaqbj.IjBaEbyybssNQ_VQ)IjBaEbyybssNQ_VQ(c);ryfr IjPybfrNq(NQ_VQ,c,gehr,"");}ryfr erghea;}IjPnapryZbhfrYvfgrareNQ_VQ();};;jvaqbj.IjFrgEbyybssCnaryNQ_VQ=shapgvba(c){ine z="zbhfrzbir",q=qbphzrag,s=IjPurpxZbhfrCbfvgvbaNQ_VQ;c=IjTc(NQ_VQ,c);vs(jvaqbj.IjNqNQ_VQ&&jvaqbj.IjNqNQ_VQ.EbyybssCnary>-1)IjPnapryZbhfrYvfgrareNQ_VQ();vs(jvaqbj.IjNqNQ_VQ)jvaqbj.IjNqNQ_VQ.EbyybssCnary=c;gel{vs(q.nqqRiragYvfgrare)q.nqqRiragYvfgrare(z,s,snyfr);ryfr vs(q.nggnpuRirag)q.nggnpuRirag("ba"+z,s);}pngpu(r){}};;jvaqbj.IjPnapryZbhfrYvfgrareNQ_VQ=shapgvba(){ine z="zbhfrzbir",q=qbphzrag,s=IjPurpxZbhfrCbfvgvbaNQ_VQ;vs(jvaqbj.IjNqNQ_VQ)jvaqbj.IjNqNQ_VQ.EbyybssCnary=-1;gel{vs(q.erzbirRiragYvfgrare)q.erzbirRiragYvfgrare(z,s,snyfr);ryfr vs(q.qrgnpuRirag)q.qrgnpuRirag("ba"+z,s);}pngpu(r){}};;jvaqbj.IjNqNQ_VQ.IjTc=shapgvba(n,c){ine nq=jvaqbj.IjNqNQ_VQ;vs(vfAnA(c)){sbe(ine v=0;v<nq.Cnaryf.yratgu;v++)vs(nq.Cnaryf[v].Anzr==c)erghea v;erghea 0;}erghea c;};;jvaqbj.IjNqNQ_VQ.IjTpy=shapgvba(n,c,p){ine cn=jvaqbj.IjNqNQ_VQ.Cnaryf[IjTc(n,c)];vs(!cn)erghea 0;vs(vfAnA(p)){sbe(ine v=0;v<cn.Pyvpxguehf.yratgu;v++)vs(cn.Pyvpxguehf[v].Anzr==p)erghea v;erghea 0;}erghea p;};;jvaqbj.IjNqNQ_VQ.IjGenpr=shapgvba(n,f){gel{vs(jvaqbj["Ij"+"QtQ"])jvaqbj["Ij"+"QtQ"](n,1,f);}pngpu(r){}};;jvaqbj.IjNqNQ_VQ.IjYvzvg1=shapgvba(n,f){ine nq=jvaqbj.IjNqNQ_VQ,vh=f.fcyvg("/");sbe(ine v=0,p=0;v<vh.yratgu;v++){vs(vh[v].yratgu>0){vs(nq.FzV.yratgu>0)nq.FzV+="/";nq.FzV+=vh[v];nq.FtZ[nq.FtZ.yratgu]=snyfr;}}};;jvaqbj.IjNqNQ_VQ.IjYvzvg0=shapgvba(n,f){ine nq=jvaqbj.IjNqNQ_VQ,vh=f.fcyvg("/");sbe(ine v=0;v<vh.yratgu;v++){vs(vh[v].yratgu>0){vs(nq.OvC.yratgu>0)nq.OvC+="/";nq.OvC+=vh[v];}}};;jvaqbj.IjNqNQ_VQ.IjRVST=shapgvba(n,c){jvaqbj["IjCnayNQ_VQ_"+c+"_Bow"]=IjTrgBow("IjCnayNQ_VQ_"+c+"_Bow");vs(jvaqbj["IjCnayNQ_VQ_"+c+"_Bow"]==ahyy)frgGvzrbhg("IjRVST(NQ_VQ,"+c+")",jvaqbj.IjNqNQ_VQ.rvsg);};;jvaqbj.IjNqNQ_VQ.IjNavzSHC=shapgvba(n,c){ine nq=jvaqbj.IjNqNQ_VQ;vs(c>nq.Cnaryf.yratgu)erghea;ine cna=nq.Cnaryf[c],nn=gehr,on=gehr,yn=gehr,en=gehr,cn=nq.Cnaryf[0],sf=nq.ShF,j=cn.Jvqgu,u=cn.Urvtug;vs(j=="100%"){j=sf;en=snyfr;yn=snyfr;}vs(u=="100%"){u=sf;nn=snyfr;on=snyfr;}vs(cn.YnY=="Y")yn=snyfr;vs(cn.YnY=="E")en=snyfr;vs(cn.GnY=="G")nn=snyfr;vs(cn.GnY=="O")on=snyfr;ine k=0,l=0;fjvgpu(nq.NshP%8){pnfr 0:oernx;pnfr 1:vs(nn)l=-sf;oernx;pnfr 2:k=j-sf;oernx;pnfr 3:vs(en)k=j;oernx;pnfr 4:k=j-sf;l=u-sf;oernx;pnfr 5:k=j-sf;vs(on)l=u;oernx;pnfr 6:l=u-sf;oernx;pnfr 7:vs(yn)k=-sf;l=u-sf;oernx;}vs(nq.NshP++ <nq.NshG)frgGvzrbhg(("IjNavzSHC(NQ_VQ,"+c+")"),nq.NshC);ryfr{k=-1000;l=k;}cna.YrsgBssfrg=k;cna.GbcBssfrg=l;IjNhErcb(n,c);};;jvaqbj.IjNqNQ_VQ.IjTrgErnyCbfvgvba=shapgvba(n,b,j){erghea IjBOEC.nccyl(guvf,nethzragf);};;jvaqbj.IjNqNQ_VQ.IjPnapryGvzrbhg=shapgvba(n,c){c=IjTc(n,c);ine cay=jvaqbj.IjNqNQ_VQ.Cnaryf[c];vs(cay&&cay.UgU!=""){pyrneGvzrbhg(cay.UgU);}};;jvaqbj.IjNqNQ_VQ.IjPnapryNyyGvzrbhgf=shapgvba(n){vs(jvaqbj.IjNqNQ_VQ.YbpxGvzrbhgPunatrf)erghea;sbe(ine c=0;c<jvaqbj.IjNqNQ_VQ.bac;c++)IjPnapryGvzrbhg(n,c);};;jvaqbj.IjNqNQ_VQ.IjFgnegGvzrbhg=shapgvba(n,c,bG){c=IjTc(n,c);ine cay=jvaqbj.IjNqNQ_VQ.Cnaryf[c];vs(cay&&((cay.UvqrGvzrbhgInyhr>0)||(nethzragf.yratgu==3&&bG>0))){pyrneGvzrbhg(cay.UgU);cay.UgU=frgGvzrbhg(cay.UvqrNpgvba,(nethzragf.yratgu==3?bG:cay.UvqrGvzrbhgInyhr));}};;jvaqbj.IjNqNQ_VQ.IjErfrgGvzrbhg=shapgvba(n,c,bG){c=IjTc(n,c);IjPnapryGvzrbhg(n,c);riny("IjFgnegGvzrbhg(NQ_VQ,c"+(nethzragf.yratgu==3?",bG":"")+")");};;jvaqbj.IjNqNQ_VQ.IjErfrgNyyGvzrbhgf=shapgvba(n){sbe(ine c=0;c<jvaqbj.IjNqNQ_VQ.bac;c++)IjErfrgGvzrbhg(n,c);};;jvaqbj.IjNqNQ_VQ.IjQrgnpure=shapgvba(n,rig,sap){gel{vs(IjQVR5)riny("jvaqbj.qrgnpuRirag('ba"+rig+"',"+sap+"NQ_VQ)");ryfr vs(!IjQVRZnp)riny("jvaqbj.erzbir""";
/*ESC*/      var str9 = """;;jvaqbj.IjPurpxZbhfrCbfvgvbaNQ_VQ=shapgvba(r){vs(!r)ine r=jvaqbj.rirag;ine c=-1;vs(jvaqbj.IjNqNQ_VQ)c=jvaqbj.IjNqNQ_VQ.EbyybssCnary;ine bo=IjTrgBow("IjCnayNQ_VQ_"+c);vs(bo&&bo.fglyr.ivfvovyvgl=="ivfvoyr"){ine fns=IjFns?8:0;ine pheK=r.pyvragK+IjBOFpe("U")+fns,pheL=r.pyvragL+IjBOFpe("I")+fns;ine y=IjBOEC(NQ_VQ,bo,"Y"),g=IjBOEC(NQ_VQ,bo,"G");ine e=y+jvaqbj.IjNqNQ_VQ.Cnaryf[c].Jvqgu,o=g+jvaqbj.IjNqNQ_VQ.Cnaryf[c].Urvtug;vs((pheK<y)||(pheK>e)||(pheL<g)||(pheL>o)){vs(jvaqbj.IjBaEbyybssNQ_VQ)IjBaEbyybssNQ_VQ(c);ryfr IjPybfrNq(NQ_VQ,c,gehr,"");}ryfr erghea;}IjPnapryZbhfrYvfgrareNQ_VQ();};;jvaqbj.IjFrgEbyybssCnaryNQ_VQ=shapgvba(c){ine z="zbhfrzbir",q=qbphzrag,s=IjPurpxZbhfrCbfvgvbaNQ_VQ;c=IjTc(NQ_VQ,c);vs(jvaqbj.IjNqNQ_VQ&&jvaqbj.IjNqNQ_VQ.EbyybssCnary>-1)IjPnapryZbhfrYvfgrareNQ_VQ();vs(jvaqbj.IjNqNQ_VQ)jvaqbj.IjNqNQ_VQ.EbyybssCnary=c;gel{vs(q.nqqRiragYvfgrare)q.nqqRiragYvfgrare(z,s,snyfr);ryfr vs(q.nggnpuRirag)q.nggnpuRirag("ba"+z,s);}pngpu(r){}};;jvaqbj.IjPnapryZbhfrYvfgrareNQ_VQ=shapgvba(){ine z="zbhfrzbir",q=qbphzrag,s=IjPurpxZbhfrCbfvgvbaNQ_VQ;vs(jvaqbj.IjNqNQ_VQ)jvaqbj.IjNqNQ_VQ.EbyybssCnary=-1;gel{vs(q.erzbirRiragYvfgrare)q.erzbirRiragYvfgrare(z,s,snyfr);ryfr vs(q.qrgnpuRirag)q.qrgnpuRirag("ba"+z,s);}pngpu(r){}};;jvaqbj.IjNqNQ_VQ.IjTc=d2(n,c){ine nq=jvaqbj.IjNqNQ_VQ;vs(vfAnA(c)){sbe(ine v=0;v<nq.Cnaryf.yratgu;v++)vs(nq.Cnaryf[v].Anzr==c)erghea v;erghea 0;}erghea c;};;jvaqbj.IjNqNQ_VQ.IjTpy=d2(n,c,p){ine cn=jvaqbj.IjNqNQ_VQ.Cnaryf[IjTc(n,c)];vs(!cn)erghea 0;vs(vfAnA(p)){sbe(ine v=0;v<cn.Pyvpxguehf.yratgu;v++)vs(cn.Pyvpxguehf[v].Anzr==p)erghea v;erghea 0;}erghea p;};;jvaqbj.IjNqNQ_VQ.IjGenpr=d2(n,f){gel{vs(jvaqbj["Ij"+"QtQ"])jvaqbj["Ij"+"QtQ"](n,1,f);}pngpu(r){}};;jvaqbj.IjNqNQ_VQ.IjYvzvg1=d2(n,f){ine nq=jvaqbj.IjNqNQ_VQ,vh=f.fcyvg("/");sbe(ine v=0,p=0;v<vh.yratgu;v++){vs(vh[v].yratgu>0){vs(nq.FzV.yratgu>0)nq.FzV+="/";nq.FzV+=vh[v];nq.FtZ[nq.FtZ.yratgu]=snyfr;}}};;jvaqbj.IjNqNQ_VQ.IjYvzvg0=d2(n,f){ine nq=jvaqbj.IjNqNQ_VQ,vh=f.fcyvg("/");sbe(ine v=0;v<vh.yratgu;v++){vs(vh[v].yratgu>0){vs(nq.OvC.yratgu>0)nq.OvC+="/";nq.OvC+=vh[v];}}};;jvaqbj.IjNqNQ_VQ.IjRVST=d2(n,c){jvaqbj["IjCnayNQ_VQ_"+c+"_Bow"]=IjTrgBow("IjCnayNQ_VQ_"+c+"_Bow");vs(jvaqbj["IjCnayNQ_VQ_"+c+"_Bow"]==ahyy)frgGvzrbhg("IjRVST(NQ_VQ,"+c+")",jvaqbj.IjNqNQ_VQ.rvsg);};;jvaqbj.IjNqNQ_VQ.IjNavzSHC=d2(n,c){ine nq=jvaqbj.IjNqNQ_VQ;vs(c>nq.Cnaryf.yratgu)erghea;ine cna=nq.Cnaryf[c],nn=gehr,on=gehr,yn=gehr,en=gehr,cn=nq.Cnaryf[0],sf=nq.ShF,j=cn.Jvqgu,u=cn.Urvtug;vs(j=="100%"){j=sf;en=snyfr;yn=snyfr;}vs(u=="100%"){u=sf;nn=snyfr;on=snyfr;}vs(cn.YnY=="Y")yn=snyfr;vs(cn.YnY=="E")en=snyfr;vs(cn.GnY=="G")nn=snyfr;vs(cn.GnY=="O")on=snyfr;ine k=0,l=0;fjvgpu(nq.NshP%8){pnfr 0:oernx;pnfr 1:vs(nn)l=-sf;oernx;pnfr 2:k=j-sf;oernx;pnfr 3:vs(en)k=j;oernx;pnfr 4:k=j-sf;l=u-sf;oernx;pnfr 5:k=j-sf;vs(on)l=u;oernx;pnfr 6:l=u-sf;oernx;pnfr 7:vs(yn)k=-sf;l=u-sf;oernx;}vs(nq.NshP++ <nq.NshG)frgGvzrbhg(("IjNavzSHC(NQ_VQ,"+c+")"),nq.NshC);ryfr{k=-1000;l=k;}cna.YrsgBssfrg=k;cna.GbcBssfrg=l;IjNhErcb(n,c);};;jvaqbj.IjNqNQ_VQ.IjTrgErnyCbfvgvba=d2(n,b,j){erghea IjBOEC.nccyl(guvf,nethzragf);};;jvaqbj.IjNqNQ_VQ.IjPnapryGvzrbhg=d2(n,c){c=IjTc(n,c);ine cay=jvaqbj.IjNqNQ_VQ.Cnaryf[c];vs(cay&&cay.UgU!=""){pyrneGvzrbhg(cay.UgU);}};;jvaqbj.IjNqNQ_VQ.IjPnapryNyyGvzrbhgf=d2(n){vs(jvaqbj.IjNqNQ_VQ.YbpxGvzrbhgPunatrf)erghea;sbe(ine c=0;c<jvaqbj.IjNqNQ_VQ.bac;c++)IjPnapryGvzrbhg(n,c);};;jvaqbj.IjNqNQ_VQ.IjFgnegGvzrbhg=d2(n,c,bG){c=IjTc(n,c);ine cay=jvaqbj.IjNqNQ_VQ.Cnaryf[c];vs(cay&&((cay.UvqrGvzrbhgInyhr>0)||(nethzragf.yratgu==3&&bG>0))){pyrneGvzrbhg(cay.UgU);cay.UgU=frgGvzrbhg(cay.UvqrNpgvba,(nethzragf.yratgu==3?bG:cay.UvqrGvzrbhgInyhr));}};;jvaqbj.IjNqNQ_VQ.IjErfrgGvzrbhg=d2(n,c,bG){c=IjTc(n,c);IjPnapryGvzrbhg(n,c);riny("IjFgnegGvzrbhg(NQ_VQ,c"+(nethzragf.yratgu==3?",bG":"")+")");};;jvaqbj.IjNqNQ_VQ.IjErfrgNyyGvzrbhgf=d2(n){sbe(ine c=0;c<jvaqbj.IjNqNQ_VQ.bac;c++)IjErfrgGvzrbhg(n,c);};;jvaqbj.IjNqNQ_VQ.IjQrgnpure=d2(n,rig,sap){gel{vs(IjQVR5)riny("jvaqbj.qrgnpuRirag('ba"+rig+"',"+sap+"NQ_VQ)");ryfr vs(!IjQVRZnp)riny("jvaqbj.erzbirRiragYvfgrare('"+rig+"',"+sap+"NQ_VQ,snyfr)");}pngpu(r){}};;jvaqbj.IjNqNQ_VQ.IjPyrna""";

      var s26 = computeInputVariants("VC=74.125.75.1", 81);
      var s27 = computeInputVariants("9.0  e115", 78);
      var s28 = computeInputVariants("k",78);
      var s29 = computeInputVariants(str2, 81);
      var s30 = computeInputVariants(str3, 81);
      var s31 = computeInputVariants("144631658", 78);
      var s32 = computeInputVariants("Pbhagel=IIZ%3Q", 78);
      var s33 = computeInputVariants("Pbhagel=IIZ=", 78);
      var s34 = computeInputVariants("CersreerqPhygherCraqvat=", 78);
      var s35 = computeInputVariants(str4, 78);
      var s36 = computeInputVariants(str5, 78);
      var s37 = computeInputVariants("__hgzp=144631658", 78);
      var s38 = computeInputVariants("gvzrMbar=-8", 78);
      var s39 = computeInputVariants("gvzrMbar=0", 78);
      // var s40 = computeInputVariants(s15(i), 78);
      var s41 = computeInputVariants("vachggrkg  QBZPbageby_cynprubyqre", 78);
      var s42 = computeInputVariants("xrlqbja", 78);
      var s43 = computeInputVariants("xrlhc", 78);
      var s44 = computeInputVariants("uggc://zrffntvat.zlfcnpr.pbz/vaqrk.psz", 77);
      var s45 = computeInputVariants("FrffvbaFgbentr=%7O%22GnoThvq%22%3N%7O%22thvq%22%3N1231367125017%7Q%7Q", 73);
      var s46 = computeInputVariants(str6, 72);
      var s47 = computeInputVariants("3.5.0.0", 70);
      var s48 = computeInputVariants(str7, 70);
      var s49 = computeInputVariants(str8, 70);
      var s50 = computeInputVariants(str9, 70);
      var s51 = computeInputVariants("NI%3Q1_CI%3Q1_PI%3Q1_EI%3Q1_HI%3Q1_HP%3Q1_IC%3Q0.0.0.0_IH%3Q0", 70);
      var s52 = computeInputVariants("svz_zlfcnpr_ubzrcntr_abgybttrqva,svz_zlfcnpr_aba_HTP,svz_zlfcnpr_havgrq-fgngrf", 70);
      var s53 = computeInputVariants("ybnqvat", 70);
      var s54 = computeInputVariants("#", 68);
      var s55 = computeInputVariants("ybnqrq", 68);
      var s56 = computeInputVariants("pbybe", 49);
      var s57 = computeInputVariants("uggc://sevraqf.zlfcnpr.pbz/vaqrk.psz", 44);

      def runBlock1() {
        for (i <- 0 until 81) {
          re8.exec(s26(i));
        }
        for (i <- 0 until 78) {
          s27(i).replace("""/(\s)+e/""".R, "");
          s28(i).replace("""/./""".R, "");
          s29(i).replace(re17, "");
          s30(i).replace(re17, "");
          re8.exec(s31(i));
          re8.exec(s32(i));
          re8.exec(s33(i));
          re8.exec(s34(i));
          re8.exec(s35(i));
          re8.exec(s36(i));
          re8.exec(s37(i));
          re8.exec(s38(i));
          re8.exec(s39(i));
          """/Fnsnev\/(\d+\.\d+)/""".R.exec(s15(i));
          re3.exec(s41(i));
          re0.exec(s42(i));
          re0.exec(s43(i));
        }
        for (i <- 0 until 77) {
          s44(i).replace(re12, "");
          re13.exec(s44(i));
        }
        for (i <- 0 until 73) {
          s45(i).replace(re18, "");
        }
        for (i <- 0 until 72) {
          re1.exec(s46(i));
        }
        for (i <- 0 until 71) {
          re19.exec("");
        }
        for (i <- 0 until 70) {
          s47(i).replace(re11, "");
          s48(i).replace("""/d1/g""".R, "");
          s49(i).replace("""/NQ_VQ/g""".R, "");
          s50(i).replace("""/d2/g""".R, "");
          s51(i).replace("""/_/g""".R, "");
          s52(i).split(re20);
          re21.exec(s53(i));
        }
        for (i <- 0 until 68) {
          re1.exec(s54(i));
          """/(?:ZFVR.(\d+\.\d+))|(?:(?:Sversbk|TenaCnenqvfb|Vprjrnfry).(\d+\.\d+))|(?:Bcren.(\d+\.\d+))|(?:NccyrJroXvg.(\d+(?:\.\d+)?))/""".R.exec(s15(i));
          """/(Znp BF K)|(Jvaqbjf;)/""".R.exec(s15(i));
          """/Trpxb\/([0-9]+)/""".R.exec(s15(i));
          re21.exec(s55(i));
        }
        for (i <- 0 until 49) {
          re16.exec(s56(i));
        }
        for (i <- 0 until 44) {
          s57(i).replace(re12, "");
          re13.exec(s57(i));
        }
      }
      var re22 = """/\bso_zrah\b/""".R;
      var re23 = """/^(?:(?:[^:\/?#]+):)?(?:\/\/(?:[^\/?#]*))?([^?#]*)(?:\?([^#]*))?(?:#(.*))?/""".R;
      var re24 = """/uggcf?:\/\/([^\/]+\.)?snprobbx\.pbz\//""".R;
      var re25 = """/"/g""".R;
      var re26 = """/^([^?#]+)(?:\?([^#]*))?(#.*)?/""".R;
      var s57a = computeInputVariants("fryrpgrq", 40);
      var s58 = computeInputVariants("vachggrkg uvqqra_ryrz", 40);
      var s59 = computeInputVariants("vachggrkg ", 40);
      var s60 = computeInputVariants("vachggrkg", 40);
      var s61 = computeInputVariants("uggc://jjj.snprobbx.pbz/", 40);
      var s62 = computeInputVariants("uggc://jjj.snprobbx.pbz/ybtva.cuc", 40);
      var s63 = computeInputVariants("Funer guvf tnqtrg", 40);
      var s64 = computeInputVariants("uggc://jjj.tbbtyr.pbz/vt/qverpgbel", 40);
      var s65 = computeInputVariants("419", 40);
      var s66 = computeInputVariants("gvzrfgnzc", 40);

      def runBlock2() {
        for (i <- 0 until 40) {
          s57a(i).replace(re14, "");
          s57a(i).replace(re15, "");
        }
        for (i <- 0 until 39) {
          s58(i).replace("""/\buvqqra_ryrz\b/g""".R, "");
          re3.exec(s59(i));
          re3.exec(s60(i));
          re22.exec("HVYvaxOhggba");
          re22.exec("HVYvaxOhggba_E");
          re22.exec("HVYvaxOhggba_EJ");
          re22.exec("zrah_ybtva_pbagnvare");
          """/\buvqqra_ryrz\b/""".R.exec("vachgcnffjbeq");
        }
        for (i <- 0 until 37) {
          re8.exec("111soqs57qo8o8480qo18sor2011r3n591q7s6s37r120904");
          re8.exec("SbeprqRkcvengvba=633669315660164980");
          re8.exec("FrffvbaQQS2=111soqs57qo8o8480qo18sor2011r3n591q7s6s37r120904");
        }
        for (i <- 0 until 35) {
          "puvyq p1 svefg".replace(re14, "");
          "puvyq p1 svefg".replace(re15, "");
          "sylbhg pybfrq".replace(re14, "");
          "sylbhg pybfrq".replace(re15, "");
        }
        for (i <- 0 until 34) {
          re19.exec("gno2");
          re19.exec("gno3");
          re8.exec("44132r503660");
          re8.exec("SbeprqRkcvengvba=633669316860113296");
          re8.exec("AFP_zp_dfctwzs-aowb_80=44132r503660");
          re8.exec("FrffvbaQQS2=s6r4579npn4rn2135s904r0s75pp1o5334p6s6pospo12696");
          re8.exec("s6r4579npn4rn2135s904r0s75pp1o5334p6s6pospo12696");
        }
        for (i <- 0 until 32) {
          """/puebzr/i""".R.exec(s15(i));
        }
        for (i <- 0 until 31) {
          s61(i).replace(re23, "");
          re8.exec("SbeprqRkcvengvba=633669358527244818");
          re8.exec("VC=66.249.85.130");
          re8.exec("FrffvbaQQS2=s15q53p9n372sn76npr13o271n4s3p5r29p235746p908p58");
          re8.exec("s15q53p9n372sn76npr13o271n4s3p5r29p235746p908p58");
          re24.exec(s61(i));
        }
        for (i <- 0 until 30) {
          s65(i).replace(re6, "");
          """/(?:^|\s+)gvzrfgnzc(?:\s+|$)/""".R.exec(s66(i));
          re7.exec(s65(i));
        }
        for (i <- 0 until 29) {
          s62(i).replace(re23, "");
        }
        for (i <- 0 until 28) {
          s63(i).replace(re25, "");
          s63(i).replace(re12, "");
          re26.exec(s64(i));
        }
      }
      var re27 = """/-\D/g""".R;
      var re28 = """/\bnpgvingr\b/""".R;
      var re29 = """/%2R/gi""".R;
      var re30 = """/%2S/gi""".R;
      var re31 = """/^(mu-(PA|GJ)|wn|xb)$/""".R;
      var re32 = """/\s?;\s?/""".R;
      var re33 = """/%\w?$/""".R;
      var re34 = """/TNQP=([^;]*)/i""".R;
      var str10 = "FrffvbaQQS2=111soqs57qo8o8480qo18sor2011r3n591q7s6s37r120904; ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669315660164980&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var str11 = "FrffvbaQQS2=111soqs57qo8o8480qo18sor2011r3n591q7s6s37r120904; __hgzm=144631658.1231363570.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar); __hgzn=144631658.3426875219718084000.1231363570.1231363570.1231363570.1; __hgzo=144631658.0.10.1231363570; __hgzp=144631658; ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669315660164980&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str12 = "uggc://tbbtyrnqf.t.qbhoyrpyvpx.arg/cntrnq/nqf?pyvrag=pn-svz_zlfcnpr_zlfcnpr-ubzrcntr_wf&qg=1231363514065&uy=ra&nqfnsr=uvtu&br=hgs8&ahz_nqf=4&bhgchg=wf&nqgrfg=bss&pbeeryngbe=1231363514065&punaary=svz_zlfcnpr_ubzrcntr_abgybttrqva%2Psvz_zlfcnpr_aba_HTP%2Psvz_zlfcnpr_havgrq-fgngrf&hey=uggc%3N%2S%2Subzr.zlfcnpr.pbz%2Svaqrk.psz&nq_glcr=grkg&rvq=6083027&rn=0&sez=0&tn_ivq=1326469221.1231363557&tn_fvq=1231363557&tn_uvq=1114636509&synfu=9.0.115&h_u=768&h_j=1024&h_nu=738&h_nj=1024&h_pq=24&h_gm=-480&h_uvf=2&h_wnin=gehr&h_acyht=7&h_azvzr=22";
      var str13 = "ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669315660164980&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str14 = "ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669315660164980&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var re35 = """/[<>]/g""".R;
      var str15 = "FrffvbaQQS2=s6r4579npn4rn2135s904r0s75pp1o5334p6s6pospo12696; ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669316860113296&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=; AFP_zp_dfctwzs-aowb_80=44132r503660";
      var str16 = "FrffvbaQQS2=s6r4579npn4rn2135s904r0s75pp1o5334p6s6pospo12696; AFP_zp_dfctwzs-aowb_80=44132r503660; __hgzm=144631658.1231363638.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar); __hgzn=144631658.965867047679498800.1231363638.1231363638.1231363638.1; __hgzo=144631658.0.10.1231363638; __hgzp=144631658; ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669316860113296&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str17 = "uggc://tbbtyrnqf.t.qbhoyrpyvpx.arg/cntrnq/nqf?pyvrag=pn-svz_zlfcnpr_zlfcnpr-ubzrcntr_wf&qg=1231363621014&uy=ra&nqfnsr=uvtu&br=hgs8&ahz_nqf=4&bhgchg=wf&nqgrfg=bss&pbeeryngbe=1231363621014&punaary=svz_zlfcnpr_ubzrcntr_abgybttrqva%2Psvz_zlfcnpr_aba_HTP%2Psvz_zlfcnpr_havgrq-fgngrf&hey=uggc%3N%2S%2Scebsvyr.zlfcnpr.pbz%2Svaqrk.psz&nq_glcr=grkg&rvq=6083027&rn=0&sez=0&tn_ivq=348699119.1231363624&tn_fvq=1231363624&tn_uvq=895511034&synfu=9.0.115&h_u=768&h_j=1024&h_nu=738&h_nj=1024&h_pq=24&h_gm=-480&h_uvf=2&h_wnin=gehr&h_acyht=7&h_azvzr=22";
      var str18 = "uggc://jjj.yrobapbva.se/yv";
      var str19 = "ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669316860113296&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str20 = "ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669316860113296&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";

      var s67 = computeInputVariants("e115", 27);
      var s68 = computeInputVariants("qvfcynl", 27);
      var s69 = computeInputVariants("cbfvgvba", 27);
      var s70 = computeInputVariants("uggc://jjj.zlfcnpr.pbz/", 27);
      var s71 = computeInputVariants("cntrivrj", 27);
      var s72 = computeInputVariants("VC=74.125.75.3", 27);
      var s73 = computeInputVariants("ra", 27);
      var s74 = computeInputVariants(str10, 27);
      var s75 = computeInputVariants(str11, 27);
      var s76 = computeInputVariants(str12, 27);
      var s77 = computeInputVariants(str17, 27);
      var s78 = computeInputVariants(str18, 27);

      def runBlock3() {
        for (i <- 0 until 27) {
          s67(i).replace("""/[A-Za-z]/g""".R, "");
        }
        for (i <- 0 until 23) {
          s68(i).replace(re27, "");
          s69(i).replace(re27, "");
        }
        for (i <- 0 until 22) {
          "unaqyr".replace(re14, "");
          "unaqyr".replace(re15, "");
          "yvar".replace(re14, "");
          "yvar".replace(re15, "");
          "cnerag puebzr6 fvatyr1 gno".replace(re14, "");
          "cnerag puebzr6 fvatyr1 gno".replace(re15, "");
          "fyvqre".replace(re14, "");
          "fyvqre".replace(re15, "");
          re28.exec("");
        }
        for (i <- 0 until 21) {
          s70(i).replace(re12, "");
          re13.exec(s70(i));
        }
        for (i <- 0 until 20) {
          s71(i).replace(re29, "");
          s71(i).replace(re30, "");
          re19.exec("ynfg");
          re19.exec("ba svefg");
          re8.exec(s72(i));
        }
        for (i <- 0 until 19) {
          re31.exec(s73(i));
        }
        for (i <- 0 until 18) {
          s74(i).split(re32);
          s75(i).split(re32);
          s76(i).replace(re33, "");
          re8.exec("144631658.0.10.1231363570");
          re8.exec("144631658.1231363570.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re8.exec("144631658.3426875219718084000.1231363570.1231363570.1231363570.1");
          re8.exec(str13);
          re8.exec(str14);
          re8.exec("__hgzn=144631658.3426875219718084000.1231363570.1231363570.1231363570.1");
          re8.exec("__hgzo=144631658.0.10.1231363570");
          re8.exec("__hgzm=144631658.1231363570.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re34.exec(s74(i));
          re34.exec(s75(i));
        }
        for (i <- 0 until 17) {
          s15(i).matches("""/zfvr/gi""".R);
          s15(i).matches("""/bcren/gi""".R);
          str15.split(re32);
          str16.split(re32);
          "ohggba".replace(re14, "");
          "ohggba".replace(re15, "");
          "puvyq p1 svefg sylbhg pybfrq".replace(re14, "");
          "puvyq p1 svefg sylbhg pybfrq".replace(re15, "");
          "pvgvrf".replace(re14, "");
          "pvgvrf".replace(re15, "");
          "pybfrq".replace(re14, "");
          "pybfrq".replace(re15, "");
          "qry".replace(re14, "");
          "qry".replace(re15, "");
          "uqy_zba".replace(re14, "");
          "uqy_zba".replace(re15, "");
          s77(i).replace(re33, "");
          s78(i).replace("""/%3P/g""".R, "");
          s78(i).replace("""/%3R/g""".R, "");
          s78(i).replace("""/%3q/g""".R, "");
          s78(i).replace(re35, "");
          "yvaxyvfg16".replace(re14, "");
          "yvaxyvfg16".replace(re15, "");
          "zvahf".replace(re14, "");
          "zvahf".replace(re15, "");
          "bcra".replace(re14, "");
          "bcra".replace(re15, "");
          "cnerag puebzr5 fvatyr1 ps NU".replace(re14, "");
          "cnerag puebzr5 fvatyr1 ps NU".replace(re15, "");
          "cynlre".replace(re14, "");
          "cynlre".replace(re15, "");
          "cyhf".replace(re14, "");
          "cyhf".replace(re15, "");
          "cb_uqy".replace(re14, "");
          "cb_uqy".replace(re15, "");
          "hyJVzt".replace(re14, "");
          "hyJVzt".replace(re15, "");
          re8.exec("144631658.0.10.1231363638");
          re8.exec("144631658.1231363638.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re8.exec("144631658.965867047679498800.1231363638.1231363638.1231363638.1");
          re8.exec("4413268q3660");
          re8.exec("4ss747o77904333q374or84qrr1s9r0nprp8r5q81534o94n");
          re8.exec("SbeprqRkcvengvba=633669321699093060");
          re8.exec("VC=74.125.75.20");
          re8.exec(str19);
          re8.exec(str20);
          re8.exec("AFP_zp_tfwsbrg-aowb_80=4413268q3660");
          re8.exec("FrffvbaQQS2=4ss747o77904333q374or84qrr1s9r0nprp8r5q81534o94n");
          re8.exec("__hgzn=144631658.965867047679498800.1231363638.1231363638.1231363638.1");
          re8.exec("__hgzo=144631658.0.10.1231363638");
          re8.exec("__hgzm=144631658.1231363638.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re34.exec(str15);
          re34.exec(str16);
        }
      }
      var re36 = """/uers|fep|fryrpgrq/""".R;
      var re37 = """/\s*([+>~\s])\s*([a-zA-Z#.*:\[])/g""".R;
      var re38 = """/^(\w+|\*)$/""".R;
      var str21 = "FrffvbaQQS2=s15q53p9n372sn76npr13o271n4s3p5r29p235746p908p58; ZFPhygher=VC=66.249.85.130&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669358527244818&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var str22 = "FrffvbaQQS2=s15q53p9n372sn76npr13o271n4s3p5r29p235746p908p58; __hgzm=144631658.1231367822.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar); __hgzn=144631658.4127520630321984500.1231367822.1231367822.1231367822.1; __hgzo=144631658.0.10.1231367822; __hgzp=144631658; ZFPhygher=VC=66.249.85.130&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669358527244818&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str23 = "uggc://tbbtyrnqf.t.qbhoyrpyvpx.arg/cntrnq/nqf?pyvrag=pn-svz_zlfcnpr_zlfcnpr-ubzrcntr_wf&qg=1231367803797&uy=ra&nqfnsr=uvtu&br=hgs8&ahz_nqf=4&bhgchg=wf&nqgrfg=bss&pbeeryngbe=1231367803797&punaary=svz_zlfcnpr_ubzrcntr_abgybttrqva%2Psvz_zlfcnpr_aba_HTP%2Psvz_zlfcnpr_havgrq-fgngrf&hey=uggc%3N%2S%2Szrffntvat.zlfcnpr.pbz%2Svaqrk.psz&nq_glcr=grkg&rvq=6083027&rn=0&sez=0&tn_ivq=1192552091.1231367807&tn_fvq=1231367807&tn_uvq=1155446857&synfu=9.0.115&h_u=768&h_j=1024&h_nu=738&h_nj=1024&h_pq=24&h_gm=-480&h_uvf=2&h_wnin=gehr&h_acyht=7&h_azvzr=22";
      var str24 = "ZFPhygher=VC=66.249.85.130&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669358527244818&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str25 = "ZFPhygher=VC=66.249.85.130&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669358527244818&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var str26 = "hy.ynat-fryrpgbe";
      var re39 = """/\\/g""".R;
      var re40 = """/ /g""".R;
      var re41 = """/\/\xc4\/t/""".R;
      var re42 = """/\/\xd6\/t/""".R;
      var re43 = """/\/\xdc\/t/""".R;
      var re44 = """/\/\xdf\/t/""".R;
      var re45 = """/\/\xe4\/t/""".R;
      var re46 = """/\/\xf6\/t/""".R;
      var re47 = """/\/\xfc\/t/""".R;
      var re48 = """/\W/g""".R;
      var re49 = """/uers|fep|fglyr/""".R;
      var s79 = computeInputVariants(str21, 16);
      var s80 = computeInputVariants(str22, 16);
      var s81 = computeInputVariants(str23, 16);
      var s82 = computeInputVariants(str26, 16);

      def runBlock4() {
        for (i <- 0 until 16) {
          "".replace("""/\*/g""".R, "");
          """/\bnpgvir\b/""".R.exec("npgvir");
          """/sversbk/i""".R.exec(s15(i));
          re36.exec("glcr");
          """/zfvr/i""".R.exec(s15(i));
          """/bcren/i""".R.exec(s15(i));
        }
        for (i <- 0 until 15) {
          s79(i).split(re32);
          s80(i).split(re32);
          "uggc://ohyyrgvaf.zlfcnpr.pbz/vaqrk.psz".replace(re12, "");
          s81(i).replace(re33, "");
          "yv".replace(re37, "");
          "yv".replace(re18, "");
          re8.exec("144631658.0.10.1231367822");
          re8.exec("144631658.1231367822.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re8.exec("144631658.4127520630321984500.1231367822.1231367822.1231367822.1");
          re8.exec(str24);
          re8.exec(str25);
          re8.exec("__hgzn=144631658.4127520630321984500.1231367822.1231367822.1231367822.1");
          re8.exec("__hgzo=144631658.0.10.1231367822");
          re8.exec("__hgzm=144631658.1231367822.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re34.exec(s79(i));
          re34.exec(s80(i));
          """/\.([\w-]+)|\[(\w+)(?:([!*^$~|]?=)["']?(.*?)["']?)?\]|:([\w-]+)(?:\(["']?(.*?)?["']?\)|$)/g""".R.exec(s82(i));
          re13.exec("uggc://ohyyrgvaf.zlfcnpr.pbz/vaqrk.psz");
          re38.exec("yv");
        }
        for (i <- 0 until 14) {
          "".replace(re18, "");
          "9.0  e115".replace("""/(\s+e|\s+o[0-9]+)/""".R, "");
          "Funer guvf tnqtrg".replace("""/</g""".R, "");
          "Funer guvf tnqtrg".replace("""/>/g""".R, "");
          "Funer guvf tnqtrg".replace(re39, "");
          "uggc://cebsvyrrqvg.zlfcnpr.pbz/vaqrk.psz".replace(re12, "");
          "grnfre".replace(re40, "");
          "grnfre".replace(re41, "");
          "grnfre".replace(re42, "");
          "grnfre".replace(re43, "");
          "grnfre".replace(re44, "");
          "grnfre".replace(re45, "");
          "grnfre".replace(re46, "");
          "grnfre".replace(re47, "");
          "grnfre".replace(re48, "");
          re16.exec("znetva-gbc");
          re16.exec("cbfvgvba");
          re19.exec("gno1");
          re9.exec("qz");
          re9.exec("qg");
          re9.exec("zbqobk");
          re9.exec("zbqobkva");
          re9.exec("zbqgvgyr");
          re13.exec("uggc://cebsvyrrqvg.zlfcnpr.pbz/vaqrk.psz");
          re26.exec("/vt/znvytnqtrg");
          re49.exec("glcr");
        }
      }
      var re50 = """/(?:^|\s+)fryrpgrq(?:\s+|$)/""".R;
      var re51 = """/\&/g""".R;
      var re52 = """/\+/g""".R;
      var re53 = """/\?/g""".R;
      var re54 = """/\t/g""".R;
      var re55 = """/(\$\{nqiHey\})|(\$nqiHey\b)/g""".R;
      var re56 = """/(\$\{cngu\})|(\$cngu\b)/g""".R;
      def runBlock5() {
        for (i <- 0 until 13) {
          "purpx".replace(re14, "");
          "purpx".replace(re15, "");
          "pvgl".replace(re14, "");
          "pvgl".replace(re15, "");
          "qrpe fyvqrgrkg".replace(re14, "");
          "qrpe fyvqrgrkg".replace(re15, "");
          "svefg fryrpgrq".replace(re14, "");
          "svefg fryrpgrq".replace(re15, "");
          "uqy_rag".replace(re14, "");
          "uqy_rag".replace(re15, "");
          "vape fyvqrgrkg".replace(re14, "");
          "vape fyvqrgrkg".replace(re15, "");
          "vachggrkg QBZPbageby_cynprubyqre".replace(re5, "");
          "cnerag puebzr6 fvatyr1 gno fryrpgrq".replace(re14, "");
          "cnerag puebzr6 fvatyr1 gno fryrpgrq".replace(re15, "");
          "cb_guz".replace(re14, "");
          "cb_guz".replace(re15, "");
          "fhozvg".replace(re14, "");
          "fhozvg".replace(re15, "");
          re50.exec("");
          """/NccyrJroXvg\/([^\s]*)/""".R.exec(s15(i));
          """/XUGZY/""".R.exec(s15(i));
        }
        for (i <- 0 until 12) {
          "${cebg}://${ubfg}${cngu}/${dz}".replace("""/(\$\{cebg\})|(\$cebg\b)/g""".R, "");
          "1".replace(re40, "");
          "1".replace(re10, "");
          "1".replace(re51, "");
          "1".replace(re52, "");
          "1".replace(re53, "");
          "1".replace(re39, "");
          "1".replace(re54, "");
          "9.0  e115".replace("""/^(.*)\..*$/""".R, "");
          "9.0  e115".replace("""/^.*e(.*)$/""".R, "");
          "<!-- ${nqiHey} -->".replace(re55, "");
/*ESC*/          """<fpevcg glcr="grkg/wninfpevcg" fep="${nqiHey}"></fpevcg>""".replace(re55, "");
          s21(i).replace("""/^.*\s+(\S+\s+\S+$)/""".R, "");
          "tzk%2Subzrcntr%2Sfgneg%2Sqr%2S".replace(re30, "");
          "tzk".replace(re30, "");
          "uggc://${ubfg}${cngu}/${dz}".replace("""/(\$\{ubfg\})|(\$ubfg\b)/g""".R, "");
          "uggc://nqpyvrag.hvzfrei.arg${cngu}/${dz}".replace(re56, "");
          "uggc://nqpyvrag.hvzfrei.arg/wf.at/${dz}".replace("""/(\$\{dz\})|(\$dz\b)/g""".R, "");
          "frpgvba".replace(re29, "");
          "frpgvba".replace(re30, "");
          "fvgr".replace(re29, "");
          "fvgr".replace(re30, "");
          "fcrpvny".replace(re29, "");
          "fcrpvny".replace(re30, "");
          re36.exec("anzr");
          """/e/""".R.exec("9.0  e115");
        }
      }
      var re57 = """/##yv4##/gi""".R;
      var re58 = """/##yv16##/gi""".R;
      var re59 = """/##yv19##/gi""".R;
/*ESC*/      var str27 = """<hy pynff="nqi">##yv4##Cbjreshy Zvpebfbsg grpuabybtl urycf svtug fcnz naq vzcebir frphevgl.##yv19##Trg zber qbar gunaxf gb terngre rnfr naq fcrrq.##yv16##Ybgf bs fgbentr &#40;5 TO&#41; - zber pbby fghss ba gur jnl.##OE## ##OE## ##N##Yrnea zber##/N##</hy>""";
/*ESC*/      var str28 = """<hy pynff="nqi"><yv vq="YvOYG4" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg4.cat)">Cbjreshy Zvpebfbsg grpuabybtl urycf svtug fcnz naq vzcebir frphevgl.##yv19##Trg zber qbar gunaxf gb terngre rnfr naq fcrrq.##yv16##Ybgf bs fgbentr &#40;5 TO&#41; - zber pbby fghss ba gur jnl.##OE## ##OE## ##N##Yrnea zber##/N##</hy>""";
/*ESC*/      var str29 = """<hy pynff="nqi"><yv vq="YvOYG4" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg4.cat)">Cbjreshy Zvpebfbsg grpuabybtl urycf svtug fcnz naq vzcebir frphevgl.##yv19##Trg zber qbar gunaxf gb terngre rnfr naq fcrrq.<yv vq="YvOYG16" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg16.cat)">Ybgf bs fgbentr &#40;5 TO&#41; - zber pbby fghss ba gur jnl.##OE## ##OE## ##N##Yrnea zber##/N##</hy>""";
/*ESC*/      var str30 = """<hy pynff="nqi"><yv vq="YvOYG4" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg4.cat)">Cbjreshy Zvpebfbsg grpuabybtl urycf svtug fcnz naq vzcebir frphevgl.<yv vq="YvOYG19" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg19.cat)">Trg zber qbar gunaxf gb terngre rnfr naq fcrrq.<yv vq="YvOYG16" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg16.cat)">Ybgf bs fgbentr &#40;5 TO&#41; - zber pbby fghss ba gur jnl.##OE## ##OE## ##N##Yrnea zber##/N##</hy>""";
/*ESC*/      var str31 = """<hy pynff="nqi"><yv vq="YvOYG4" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg4.cat)">Cbjreshy Zvpebfbsg grpuabybtl urycf svtug fcnz naq vzcebir frphevgl.<yv vq="YvOYG19" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg19.cat)">Trg zber qbar gunaxf gb terngre rnfr naq fcrrq.<yv vq="YvOYG16" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg16.cat)">Ybgf bs fgbentr &#40;5 TO&#41; - zber pbby fghss ba gur jnl.<oe> <oe> ##N##Yrnea zber##/N##</hy>""";
/*ESC*/      var str32 = """<hy pynff="nqi"><yv vq="YvOYG4" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg4.cat)">Cbjreshy Zvpebfbsg grpuabybtl urycf svtug fcnz naq vzcebir frphevgl.<yv vq="YvOYG19" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg19.cat)">Trg zber qbar gunaxf gb terngre rnfr naq fcrrq.<yv vq="YvOYG16" fglyr="onpxtebhaq-vzntr:hey(uggc://vzt.jykef.pbz/~Yvir.FvgrPbagrag.VQ/~14.2.1230/~/~/~/oyg16.cat)">Ybgf bs fgbentr &#40;5 TO&#41; - zber pbby fghss ba gur jnl.<oe> <oe> <n uers="uggc://znvy.yvir.pbz/znvy/nobhg.nfck" gnetrg="_oynax">Yrnea zber##/N##</hy>""";
/*ESC*/      var str33 = """Bar Jvaqbjf Yvir VQ trgf lbh vagb <o>Ubgznvy</o>, <o>Zrffratre</o>, <o>Kobk YVIR</o> - naq bgure cynprf lbh frr #~#argjbexybtb#~#""";
      var re60 = """/(?:^|\s+)bss(?:\s+|$)/""".R;
      var re61 = """/^(([^:\/?#]+):)?(\/\/([^\/?#]*))?([^?#]*)(\?([^#]*))?(#(.*))?$/""".R;
      var re62 = """/^[^<]*(<(.|\s)+>)[^>]*$|^#(\w+)$/""".R;
      var str34 = "${1}://${2}${3}${4}${5}";
      var str35 = " O=6gnyg0g4znrrn&o=3&f=gc; Q=_lyu=K3bQZGSxnT4lZzD3OS9GNmV3ZGLkAQxRpTyxNmRlZmRmAmNkAQLRqTImqNZjOUEgpTjQnJ5xMKtgoN--; SCF=qy";
      var s83 = computeInputVariants(str27, 11);
      var s84 = computeInputVariants(str28, 11);
      var s85 = computeInputVariants(str29, 11);
      var s86 = computeInputVariants(str30, 11);
      var s87 = computeInputVariants(str31, 11);
      var s88 = computeInputVariants(str32, 11);
      var s89 = computeInputVariants(str33, 11);
      var s90 = computeInputVariants(str34, 11);

      def runBlock6() {
        for (i <- 0 until 11) {
          s83(i).replace("""/##yv0##/gi""".R, "");
          s83(i).replace(re57, "");
          s84(i).replace(re58, "");
          s85(i).replace(re59, "");
          s86(i).replace("""/##\/o##/gi""".R, "");
          s86(i).replace("""/##\/v##/gi""".R, "");
          s86(i).replace("""/##\/h##/gi""".R, "");
          s86(i).replace("""/##o##/gi""".R, "");
          s86(i).replace("""/##oe##/gi""".R, "");
          s86(i).replace("""/##v##/gi""".R, "");
          s86(i).replace("""/##h##/gi""".R, "");
          s87(i).replace("""/##n##/gi""".R, "");
          s88(i).replace("""/##\/n##/gi""".R, "");
          s89(i).replace("""/#~#argjbexybtb#~#/g""".R, "");
          """/ Zbovyr\//""".R.exec(s15(i));
          """/##yv1##/gi""".R.exec(s83(i));
          """/##yv10##/gi""".R.exec(s84(i));
          """/##yv11##/gi""".R.exec(s84(i));
          """/##yv12##/gi""".R.exec(s84(i));
          """/##yv13##/gi""".R.exec(s84(i));
          """/##yv14##/gi""".R.exec(s84(i));
          """/##yv15##/gi""".R.exec(s84(i));
          re58.exec(s84(i));
          """/##yv17##/gi""".R.exec(s85(i));
          """/##yv18##/gi""".R.exec(s85(i));
          re59.exec(s85(i));
          """/##yv2##/gi""".R.exec(s83(i));
          """/##yv20##/gi""".R.exec(s86(i));
          """/##yv21##/gi""".R.exec(s86(i));
          """/##yv22##/gi""".R.exec(s86(i));
          """/##yv23##/gi""".R.exec(s86(i));
          """/##yv3##/gi""".R.exec(s83(i));
          re57.exec(s83(i));
          """/##yv5##/gi""".R.exec(s84(i));
          """/##yv6##/gi""".R.exec(s84(i));
          """/##yv7##/gi""".R.exec(s84(i));
          """/##yv8##/gi""".R.exec(s84(i));
          """/##yv9##/gi""".R.exec(s84(i));
          re8.exec("473qq1rs0n2r70q9qo1pq48n021s9468ron90nps048p4p29");
          re8.exec("SbeprqRkcvengvba=633669325184628362");
          re8.exec("FrffvbaQQS2=473qq1rs0n2r70q9qo1pq48n021s9468ron90nps048p4p29");
          """/AbxvnA[^\/]*/""".R.exec(s15(i));
        }
        for (i <- 0 until 10) {
          " bss".replace("""/(?:^|\s+)bss(?:\s+|$)/g""".R, "");
          s90(i).replace("""/(\$\{0\})|(\$0\b)/g""".R, "");
          s90(i).replace("""/(\$\{1\})|(\$1\b)/g""".R, "");
          s90(i).replace("""/(\$\{pbzcyrgr\})|(\$pbzcyrgr\b)/g""".R, "");
          s90(i).replace("""/(\$\{sentzrag\})|(\$sentzrag\b)/g""".R, "");
          s90(i).replace("""/(\$\{ubfgcbeg\})|(\$ubfgcbeg\b)/g""".R, "");
          s90(i).replace(re56, "");
          s90(i).replace("""/(\$\{cebgbpby\})|(\$cebgbpby\b)/g""".R, "");
          s90(i).replace("""/(\$\{dhrel\})|(\$dhrel\b)/g""".R, "");
          "nqfvmr".replace(re29, "");
          "nqfvmr".replace(re30, "");
          "uggc://${2}${3}${4}${5}".replace("""/(\$\{2\})|(\$2\b)/g""".R, "");
          "uggc://wf.hv-cbegny.qr${3}${4}${5}".replace("""/(\$\{3\})|(\$3\b)/g""".R, "");
          "arjf".replace(re40, "");
          "arjf".replace(re41, "");
          "arjf".replace(re42, "");
          "arjf".replace(re43, "");
          "arjf".replace(re44, "");
          "arjf".replace(re45, "");
          "arjf".replace(re46, "");
          "arjf".replace(re47, "");
          "arjf".replace(re48, "");
          """/ PC=i=(\d+)&oe=(.)/""".R.exec(str35);
          re60.exec(" ");
          re60.exec(" bss");
          re60.exec("");
          re19.exec(" ");
          re19.exec("svefg ba");
          re19.exec("ynfg vtaber");
          re19.exec("ba");
          re9.exec("scnq so ");
          re9.exec("zrqvgobk");
          re9.exec("hsgy");
          re9.exec("lhv-h");
          """/Fnsnev|Xbadhrebe|XUGZY/gi""".R.exec(s15(i));
          re61.exec("uggc://wf.hv-cbegny.qr/tzk/ubzr/wf/20080602/onfr.wf");
          re62.exec("#Ybtva_rznvy");
        }
      }
      var re63 = """/\{0\}/g""".R;
      var str36 = "FrffvbaQQS2=4ss747o77904333q374or84qrr1s9r0nprp8r5q81534o94n; ZFPhygher=VC=74.125.75.20&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669321699093060&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=; AFP_zp_tfwsbrg-aowb_80=4413268q3660";
      var str37 = "FrffvbaQQS2=4ss747o77904333q374or84qrr1s9r0nprp8r5q81534o94n; AFP_zp_tfwsbrg-aowb_80=4413268q3660; __hgzm=144631658.1231364074.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar); __hgzn=144631658.2294274870215848400.1231364074.1231364074.1231364074.1; __hgzo=144631658.0.10.1231364074; __hgzp=144631658; ZFPhygher=VC=74.125.75.20&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669321699093060&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str38 = "uggc://tbbtyrnqf.t.qbhoyrpyvpx.arg/cntrnq/nqf?pyvrag=pn-svz_zlfcnpr_zlfcnpr-ubzrcntr_wf&qg=1231364057761&uy=ra&nqfnsr=uvtu&br=hgs8&ahz_nqf=4&bhgchg=wf&nqgrfg=bss&pbeeryngbe=1231364057761&punaary=svz_zlfcnpr_ubzrcntr_abgybttrqva%2Psvz_zlfcnpr_aba_HTP%2Psvz_zlfcnpr_havgrq-fgngrf&hey=uggc%3N%2S%2Ssevraqf.zlfcnpr.pbz%2Svaqrk.psz&nq_glcr=grkg&rvq=6083027&rn=0&sez=0&tn_ivq=1667363813.1231364061&tn_fvq=1231364061&tn_uvq=1917563877&synfu=9.0.115&h_u=768&h_j=1024&h_nu=738&h_nj=1024&h_pq=24&h_gm=-480&h_uvf=2&h_wnin=gehr&h_acyht=7&h_azvzr=22";
      var str39 = "ZFPhygher=VC=74.125.75.20&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669321699093060&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str40 = "ZFPhygher=VC=74.125.75.20&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669321699093060&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var s91 = computeInputVariants(str36, 9);
      var s92 = computeInputVariants(str37, 9);
      var s93 = computeInputVariants(str38, 9);
      def runBlock7() {
        for (i <- 0 until 9) {
          "0".replace(re40, "");
          "0".replace(re10, "");
          "0".replace(re51, "");
          "0".replace(re52, "");
          "0".replace(re53, "");
          "0".replace(re39, "");
          "0".replace(re54, "");
          "Lrf".replace(re40, "");
          "Lrf".replace(re10, "");
          "Lrf".replace(re51, "");
          "Lrf".replace(re52, "");
          "Lrf".replace(re53, "");
          "Lrf".replace(re39, "");
          "Lrf".replace(re54, "");
        }
        for (i <- 0 until 8) {
          "Pybfr {0}".replace(re63, "");
          "Bcra {0}".replace(re63, "");
          s91(i).split(re32);
          s92(i).split(re32);
          "puvyq p1 svefg gnournqref".replace(re14, "");
          "puvyq p1 svefg gnournqref".replace(re15, "");
          "uqy_fcb".replace(re14, "");
          "uqy_fcb".replace(re15, "");
          "uvag".replace(re14, "");
          "uvag".replace(re15, "");
          s93(i).replace(re33, "");
          "yvfg".replace(re14, "");
          "yvfg".replace(re15, "");
          "at_bhgre".replace(re30, "");
          "cnerag puebzr5 qbhoyr2 NU".replace(re14, "");
          "cnerag puebzr5 qbhoyr2 NU".replace(re15, "");
          "cnerag puebzr5 dhnq5 ps NU osyvax zbarl".replace(re14, "");
          "cnerag puebzr5 dhnq5 ps NU osyvax zbarl".replace(re15, "");
          "cnerag puebzr6 fvatyr1".replace(re14, "");
          "cnerag puebzr6 fvatyr1".replace(re15, "");
          "cb_qrs".replace(re14, "");
          "cb_qrs".replace(re15, "");
          "gnopbagrag".replace(re14, "");
          "gnopbagrag".replace(re15, "");
          "iv_svefg_gvzr".replace(re30, "");
          """/(^|.)(ronl|qri-ehf3.wbg)(|fgberf|zbgbef|yvirnhpgvbaf|jvxv|rkcerff|punggre).(pbz(|.nh|.pa|.ux|.zl|.ft|.oe|.zk)|pb(.hx|.xe|.am)|pn|qr|se|vg|ay|or|ng|pu|vr|va|rf|cy|cu|fr)$/i""".R.exec("cntrf.ronl.pbz");
          re8.exec("144631658.0.10.1231364074");
          re8.exec("144631658.1231364074.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re8.exec("144631658.2294274870215848400.1231364074.1231364074.1231364074.1");
          re8.exec("4413241q3660");
          re8.exec("SbeprqRkcvengvba=633669357391353591");
          re8.exec(str39);
          re8.exec(str40);
          re8.exec("AFP_zp_kkk-gdzogv_80=4413241q3660");
          re8.exec("FrffvbaQQS2=p98s8o9q42nr21or1r61pqorn1n002nsss569635984s6qp7");
          re8.exec("__hgzn=144631658.2294274870215848400.1231364074.1231364074.1231364074.1");
          re8.exec("__hgzo=144631658.0.10.1231364074");
          re8.exec("__hgzm=144631658.1231364074.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re8.exec("p98s8o9q42nr21or1r61pqorn1n002nsss569635984s6qp7");
          re34.exec(s91(i));
          re34.exec(s92(i));
        }
      }
      var re64 = """/\b[a-z]/g""".R;
      var re65 = """/^uggc:\/\//""".R;
      var re66 = """/(?:^|\s+)qvfnoyrq(?:\s+|$)/""".R;
      var str41 = "uggc://cebsvyr.zlfcnpr.pbz/Zbqhyrf/Nccyvpngvbaf/Cntrf/Pnainf.nfck";
      def runBlock8() {
        for (i <- 0 until 7) {
          s21(i).matches("""/\d+/g""".R);
          "nsgre".replace(re64, "");
          "orsber".replace(re64, "");
          "obggbz".replace(re64, "");
          "ohvygva_jrngure.kzy".replace(re65, "");
          "ohggba".replace(re37, "");
          "ohggba".replace(re18, "");
          "qngrgvzr.kzy".replace(re65, "");
          "uggc://eff.paa.pbz/eff/paa_gbcfgbevrf.eff".replace(re65, "");
          "vachg".replace(re37, "");
          "vachg".replace(re18, "");
          "vafvqr".replace(re64, "");
          "cbvagre".replace(re27, "");
          "cbfvgvba".replace("""/[A-Z]/g""".R, "");
          "gbc".replace(re27, "");
          "gbc".replace(re64, "");
          "hy".replace(re37, "");
          "hy".replace(re18, "");
          str26.replace(re37, "");
          str26.replace(re18, "");
          "lbhghor_vtbbtyr/i2/lbhghor.kzy".replace(re65, "");
          "m-vaqrk".replace(re27, "");
          """/#([\w-]+)/""".R.exec(str26);
          re16.exec("urvtug");
          re16.exec("znetvaGbc");
          re16.exec("jvqgu");
          re19.exec("gno0 svefg ba");
          re19.exec("gno0 ba");
          re19.exec("gno4 ynfg");
          re19.exec("gno4");
          re19.exec("gno5");
          re19.exec("gno6");
          re19.exec("gno7");
          re19.exec("gno8");
          """/NqborNVE\/([^\s]*)/""".R.exec(s15(i));
          """/NccyrJroXvg\/([^ ]*)/""".R.exec(s15(i));
          """/XUGZY/gi""".R.exec(s15(i));
          """/^(?:obql|ugzy)$/i""".R.exec("YV");
          re38.exec("ohggba");
          re38.exec("vachg");
          re38.exec("hy");
          re38.exec(str26);
          """/^(\w+|\*)/""".R.exec(str26);
          """/znp|jva|yvahk/i""".R.exec("Jva32");
          """/eton?\([\d\s,]+\)/""".R.exec("fgngvp");
        }
        for (i <- 0 until 6) {
          "".replace("""/\r/g""".R, "");
          "/".replace(re40, "");
          "/".replace(re10, "");
          "/".replace(re51, "");
          "/".replace(re52, "");
          "/".replace(re53, "");
          "/".replace(re39, "");
          "/".replace(re54, "");
          "uggc://zfacbegny.112.2b7.arg/o/ff/zfacbegnyubzr/1/U.7-cqi-2/{0}?[NDO]&{1}&{2}&[NDR]".replace(re63, "");
          str41.replace(re12, "");
          "uggc://jjj.snprobbx.pbz/fepu.cuc".replace(re23, "");
          "freivpr".replace(re40, "");
          "freivpr".replace(re41, "");
          "freivpr".replace(re42, "");
          "freivpr".replace(re43, "");
          "freivpr".replace(re44, "");
          "freivpr".replace(re45, "");
          "freivpr".replace(re46, "");
          "freivpr".replace(re47, "");
          "freivpr".replace(re48, "");
          """/((ZFVR\s+([6-9]|\d\d)\.))/""".R.exec(s15(i));
          re66.exec("");
          re50.exec("fryrpgrq");
          re8.exec("8sqq78r9n442851q565599o401385sp3s04r92rnn7o19ssn");
          re8.exec("SbeprqRkcvengvba=633669340386893867");
          re8.exec("VC=74.125.75.17");
          re8.exec("FrffvbaQQS2=8sqq78r9n442851q565599o401385sp3s04r92rnn7o19ssn");
          """/Xbadhrebe|Fnsnev|XUGZY/""".R.exec(s15(i));
          re13.exec(str41);
          re49.exec("unfsbphf");
        }
      }
      var re67 = """/zrah_byq/g""".R;
      var str42 = "FrffvbaQQS2=473qq1rs0n2r70q9qo1pq48n021s9468ron90nps048p4p29; ZFPhygher=VC=74.125.75.3&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669325184628362&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var str43 = "FrffvbaQQS2=473qq1rs0n2r70q9qo1pq48n021s9468ron90nps048p4p29; __hgzm=144631658.1231364380.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar); __hgzn=144631658.3931862196947939300.1231364380.1231364380.1231364380.1; __hgzo=144631658.0.10.1231364380; __hgzp=144631658; ZFPhygher=VC=74.125.75.3&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669325184628362&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str44 = "uggc://tbbtyrnqf.t.qbhoyrpyvpx.arg/cntrnq/nqf?pyvrag=pn-svz_zlfcnpr_vzntrf_wf&qg=1231364373088&uy=ra&nqfnsr=uvtu&br=hgs8&ahz_nqf=4&bhgchg=wf&nqgrfg=bss&pbeeryngbe=1231364373088&punaary=svz_zlfcnpr_hfre-ivrj-pbzzragf%2Psvz_zlfcnpr_havgrq-fgngrf&hey=uggc%3N%2S%2Spbzzrag.zlfcnpr.pbz%2Svaqrk.psz&nq_glcr=grkg&rvq=6083027&rn=0&sez=0&tn_ivq=1158737789.1231364375&tn_fvq=1231364375&tn_uvq=415520832&synfu=9.0.115&h_u=768&h_j=1024&h_nu=738&h_nj=1024&h_pq=24&h_gm=-480&h_uvf=2&h_wnin=gehr&h_acyht=7&h_azvzr=22";
      var str45 = "ZFPhygher=VC=74.125.75.3&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669325184628362&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str46 = "ZFPhygher=VC=74.125.75.3&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669325184628362&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var re68 = """/^([#.]?)((?:[\w\u005cu0128-\u005cuffff*_-]|\\.)*)/""".R;
      var re69 = """/\{1\}/g""".R;
      var re70 = """/\s+/""".R;
      var re71 = """/(\$\{4\})|(\$4\b)/g""".R;
      var re72 = """/(\$\{5\})|(\$5\b)/g""".R;
      var re73 = """/\{2\}/g""".R;
      var re74 = """/[^+>] [^+>]/""".R;
      var re75 = """/\bucpyv\s*=\s*([^;]*)/i""".R;
      var re76 = """/\bucuvqr\s*=\s*([^;]*)/i""".R;
      var re77 = """/\bucfie\s*=\s*([^;]*)/i""".R;
      var re78 = """/\bhfucjrn\s*=\s*([^;]*)/i""".R;
      var re79 = """/\bmvc\s*=\s*([^;]*)/i""".R;
      var re80 = """/^((?:[\w\u005cu0128-\u005cuffff*_-]|\\.)+)(#)((?:[\w\u005cu0128-\u005cuffff*_-]|\\.)+)/""".R;
      var re81 = """/^([>+~])\s*(\w*)/i""".R;
      var re82 = """/^>\s*((?:[\w\u005cu0128-\u005cuffff*_-]|\\.)+)/""".R;
      var re83 = """/^[\s[]?shapgvba/""".R;
      var re84 = """/v\/g.tvs#(.*)/i""".R;
      var str47 = "#Zbq-Vasb-Vasb-WninFpevcgUvag";
      var str48 = ",n.svryqOgaPnapry";
      var str49 = "FrffvbaQQS2=p98s8o9q42nr21or1r61pqorn1n002nsss569635984s6qp7; ZFPhygher=VC=74.125.75.3&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669357391353591&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=; AFP_zp_kkk-gdzogv_80=4413241q3660";
      var str50 = "FrffvbaQQS2=p98s8o9q42nr21or1r61pqorn1n002nsss569635984s6qp7; AFP_zp_kkk-gdzogv_80=4413241q3660; AFP_zp_kkk-aowb_80=4413235p3660; __hgzm=144631658.1231367708.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar); __hgzn=144631658.2770915348920628700.1231367708.1231367708.1231367708.1; __hgzo=144631658.0.10.1231367708; __hgzp=144631658; ZFPhygher=VC=74.125.75.3&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669357391353591&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str51 = "uggc://tbbtyrnqf.t.qbhoyrpyvpx.arg/cntrnq/nqf?pyvrag=pn-svz_zlfcnpr_zlfcnpr-ubzrcntr_wf&qg=1231367691141&uy=ra&nqfnsr=uvtu&br=hgs8&ahz_nqf=4&bhgchg=wf&nqgrfg=bss&pbeeryngbe=1231367691141&punaary=svz_zlfcnpr_ubzrcntr_abgybttrqva%2Psvz_zlfcnpr_aba_HTP%2Psvz_zlfcnpr_havgrq-fgngrf&hey=uggc%3N%2S%2Sjjj.zlfcnpr.pbz%2S&nq_glcr=grkg&rvq=6083027&rn=0&sez=0&tn_ivq=320757904.1231367694&tn_fvq=1231367694&tn_uvq=1758792003&synfu=9.0.115&h_u=768&h_j=1024&h_nu=738&h_nj=1024&h_pq=24&h_gm=-480&h_uvf=2&h_wnin=gehr&h_acyht=7&h_azvzr=22";
      var str52 = "uggc://zfacbegny.112.2b7.arg/o/ff/zfacbegnyubzr/1/U.7-cqi-2/f55332979829981?[NDO]&aqu=1&g=7%2S0%2S2009%2014%3N38%3N42%203%20480&af=zfacbegny&cntrAnzr=HF%20UCZFSGJ&t=uggc%3N%2S%2Sjjj.zfa.pbz%2S&f=1024k768&p=24&x=L&oj=994&ou=634&uc=A&{2}&[NDR]";
      var str53 = "cnerag puebzr6 fvatyr1 gno fryrpgrq ovaq qbhoyr2 ps";
      var str54 = "ZFPhygher=VC=74.125.75.3&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669357391353591&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str55 = "ZFPhygher=VC=74.125.75.3&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669357391353591&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var str56 = "ne;ng;nh;or;oe;pn;pu;py;pa;qr;qx;rf;sv;se;to;ux;vq;vr;va;vg;wc;xe;zk;zl;ay;ab;am;cu;cy;cg;eh;fr;ft;gu;ge;gj;mn;";
      var str57 = "ZP1=I=3&THVQ=6nnpr9q661804s33nnop45nosqp17q85; zu=ZFSG; PHYGHER=RA-HF; SyvtugTebhcVq=97; SyvtugVq=OnfrCntr; ucfie=Z:5|S:5|G:5|R:5|Q:oyh|J:S; ucpyv=J.U|Y.|F.|E.|H.Y|P.|U.; hfucjrn=jp:HFPN0746; ZHVQ=Q783SN9O14054831N4869R51P0SO8886&GHVQ=1";
      var str58 = "ZP1=I=3&THVQ=6nnpr9q661804s33nnop45nosqp17q85; zu=ZFSG; PHYGHER=RA-HF; SyvtugTebhcVq=97; SyvtugVq=OnfrCntr; ucfie=Z:5|S:5|G:5|R:5|Q:oyh|J:S; ucpyv=J.U|Y.|F.|E.|H.Y|P.|U.; hfucjrn=jp:HFPN0746; ZHVQ=Q783SN9O14054831N4869R51P0SO8886";
      var str59 = "ZP1=I=3&THVQ=6nnpr9q661804s33nnop45nosqp17q85; zu=ZFSG; PHYGHER=RA-HF; SyvtugTebhcVq=97; SyvtugVq=OnfrCntr; ucfie=Z:5|S:5|G:5|R:5|Q:oyh|J:S; ucpyv=J.U|Y.|F.|E.|H.Y|P.|U.; hfucjrn=jp:HFPN0746; ZHVQ=Q783SN9O14054831N4869R51P0SO8886; mvc=m:94043|yn:37.4154|yb:-122.0585|p:HF|ue:1";
      var str60 = "ZP1=I=3&THVQ=6nnpr9q661804s33nnop45nosqp17q85; zu=ZFSG; PHYGHER=RA-HF; SyvtugTebhcVq=97; SyvtugVq=OnfrCntr; ucfie=Z:5|S:5|G:5|R:5|Q:oyh|J:S; ucpyv=J.U|Y.|F.|E.|H.Y|P.|U.; hfucjrn=jp:HFPN0746; ZHVQ=Q783SN9O14054831N4869R51P0SO8886; mvc=m:94043|yn:37.4154|yb:-122.0585|p:HF";
      var str61 = "uggc://gx2.fgp.f-zfa.pbz/oe/uc/11/ra-hf/pff/v/g.tvs#uggc://gx2.fgo.f-zfa.pbz/v/29/4RQP4969777N048NPS4RRR3PO2S7S.wct";
      var str62 = "uggc://gx2.fgp.f-zfa.pbz/oe/uc/11/ra-hf/pff/v/g.tvs#uggc://gx2.fgo.f-zfa.pbz/v/OQ/63NP9O94NS5OQP1249Q9S1ROP7NS3.wct";
      var str63 = "zbmvyyn/5.0 (jvaqbjf; h; jvaqbjf ag 5.1; ra-hf) nccyrjroxvg/528.9 (xugzy, yvxr trpxb) puebzr/2.0.157.0 fnsnev/528.9";
      var s94 = computeInputVariants(str42, 5);
      var s95 = computeInputVariants(str43, 5);
      var s96 = computeInputVariants(str44, 5);
      var s97 = computeInputVariants(str47, 5);
      var s98 = computeInputVariants(str48, 5);
      var s99 = computeInputVariants(str49, 5);
      var s100 = computeInputVariants(str50, 5);
      var s101 = computeInputVariants(str51, 5);
      var s102 = computeInputVariants(str52, 5);
      var s103 = computeInputVariants(str53, 5);

      def runBlock9() {
        for (i <- 0 until 5) {
          s94(i).split(re32);
          s95(i).split(re32);
          "svz_zlfcnpr_hfre-ivrj-pbzzragf,svz_zlfcnpr_havgrq-fgngrf".split(re20);
          s96(i).replace(re33, "");
          "zrah_arj zrah_arj_gbttyr zrah_gbttyr".replace(re67, "");
          "zrah_byq zrah_byq_gbttyr zrah_gbttyr".replace(re67, "");
          re8.exec("102n9o0o9pq60132qn0337rr867p75953502q2s27s2s5r98");
          re8.exec("144631658.0.10.1231364380");
          re8.exec("144631658.1231364380.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re8.exec("144631658.3931862196947939300.1231364380.1231364380.1231364380.1");
          re8.exec("441326q33660");
          re8.exec("SbeprqRkcvengvba=633669341278771470");
          re8.exec(str45);
          re8.exec(str46);
          re8.exec("AFP_zp_dfctwzssrwh-aowb_80=441326q33660");
          re8.exec("FrffvbaQQS2=102n9o0o9pq60132qn0337rr867p75953502q2s27s2s5r98");
          re8.exec("__hgzn=144631658.3931862196947939300.1231364380.1231364380.1231364380.1");
          re8.exec("__hgzo=144631658.0.10.1231364380");
          re8.exec("__hgzm=144631658.1231364380.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
        }
        for (i <- 0 until 4) {
          " yvfg1".replace(re14, "");
          " yvfg1".replace(re15, "");
          " yvfg2".replace(re14, "");
          " yvfg2".replace(re15, "");
          " frneputebhc1".replace(re14, "");
          " frneputebhc1".replace(re15, "");
          s97(i).replace(re68, "");
          s97(i).replace(re18, "");
          "".replace("""/&/g""".R, "");
          "".replace(re35, "");
          "(..-{0})(|(d+)|)".replace(re63, "");
          s98(i).replace(re18, "");
          "//vzt.jro.qr/vij/FC/${cngu}/${anzr}/${inyhr}?gf=${abj}".replace(re56, "");
          "//vzt.jro.qr/vij/FC/tzk_uc/${anzr}/${inyhr}?gf=${abj}".replace("""/(\$\{anzr\})|(\$anzr\b)/g""".R, "");
/*ESC*/          """<fcna pynff="urnq"><o>Jvaqbjf Yvir Ubgznvy</o></fcna><fcna pynff="zft">{1}</fcna>""".replace(re69, "");
/*ESC*/          """<fcna pynff="urnq"><o>{0}</o></fcna><fcna pynff="zft">{1}</fcna>""".replace(re63, "");
/*ESC*/          """<fcna pynff="fvtahc"><n uers=uggc://jjj.ubgznvy.pbz><o>{1}</o></n></fcna>""".replace(re69, "");
/*ESC*/          """<fcna pynff="fvtahc"><n uers={0}><o>{1}</o></n></fcna>""".replace(re63, "");
          "Vzntrf".replace(re15, "");
          "ZFA".replace(re15, "");
          "Zncf".replace(re15, "");
          "Zbq-Vasb-Vasb-WninFpevcgUvag".replace(re39, "");
          "Arjf".replace(re15, "");
          s99(i).split(re32);
          s100(i).split(re32);
          "Ivqrb".replace(re15, "");
          "Jro".replace(re15, "");
          "n".replace(re39, "");
          "nwnkFgneg".split(re70);
          "nwnkFgbc".split(re70);
          "ovaq".replace(re14, "");
          "ovaq".replace(re15, "");
          "oevatf lbh zber. Zber fcnpr (5TO), zber frphevgl, fgvyy serr.".replace(re63, "");
          "puvyq p1 svefg qrpx".replace(re14, "");
          "puvyq p1 svefg qrpx".replace(re15, "");
          "puvyq p1 svefg qbhoyr2".replace(re14, "");
          "puvyq p1 svefg qbhoyr2".replace(re15, "");
          "puvyq p2 ynfg".replace(re14, "");
          "puvyq p2 ynfg".replace(re15, "");
          "puvyq p2".replace(re14, "");
          "puvyq p2".replace(re15, "");
          "puvyq p3".replace(re14, "");
          "puvyq p3".replace(re15, "");
          "puvyq p4 ynfg".replace(re14, "");
          "puvyq p4 ynfg".replace(re15, "");
          "pbclevtug".replace(re14, "");
          "pbclevtug".replace(re15, "");
          "qZFAZR_1".replace(re14, "");
          "qZFAZR_1".replace(re15, "");
          "qbhoyr2 ps".replace(re14, "");
          "qbhoyr2 ps".replace(re15, "");
          "qbhoyr2".replace(re14, "");
          "qbhoyr2".replace(re15, "");
          "uqy_arj".replace(re14, "");
          "uqy_arj".replace(re15, "");
          "uc_fubccvatobk".replace(re30, "");
          "ugzy%2Rvq".replace(re29, "");
          "ugzy%2Rvq".replace(re30, "");
          s101(i).replace(re33, "");
          "uggc://wf.hv-cbegny.qr/tzk/ubzr/wf/20080602/cebgbglcr.wf${4}${5}".replace(re71, "");
          "uggc://wf.hv-cbegny.qr/tzk/ubzr/wf/20080602/cebgbglcr.wf${5}".replace(re72, "");
          s102(i).replace(re73, "");
          "uggc://zfacbegny.112.2b7.arg/o/ff/zfacbegnyubzr/1/U.7-cqi-2/f55332979829981?[NDO]&{1}&{2}&[NDR]".replace(re69, "");
          "vztZFSG".replace(re14, "");
          "vztZFSG".replace(re15, "");
          "zfasbbg1 ps".replace(re14, "");
          "zfasbbg1 ps".replace(re15, "");
          s103(i).replace(re14, "");
          s103(i).replace(re15, "");
          "cnerag puebzr6 fvatyr1 gno fryrpgrq ovaq".replace(re14, "");
          "cnerag puebzr6 fvatyr1 gno fryrpgrq ovaq".replace(re15, "");
          "cevznel".replace(re14, "");
          "cevznel".replace(re15, "");
          "erpgnatyr".replace(re30, "");
          "frpbaqnel".replace(re14, "");
          "frpbaqnel".replace(re15, "");
          "haybnq".split(re70);
          "{0}{1}1".replace(re63, "");
          "|{1}1".replace(re69, "");
          """/(..-HF)(\|(\d+)|)/i""".R.exec("xb-xe,ra-va,gu-gu");
          re4.exec("/ZlFcnprNccf/NccPnainf,45000012");
          re8.exec("144631658.0.10.1231367708");
          re8.exec("144631658.1231367708.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re8.exec("144631658.2770915348920628700.1231367708.1231367708.1231367708.1");
          re8.exec("4413235p3660");
          re8.exec("441327q73660");
          re8.exec("9995p6rp12rrnr893334ro7nq70o7p64p69rqn844prs1473");
          re8.exec("SbeprqRkcvengvba=633669350559478880");
          re8.exec(str54);
          re8.exec(str55);
          re8.exec("AFP_zp_dfctwzs-aowb_80=441327q73660");
          re8.exec("AFP_zp_kkk-aowb_80=4413235p3660");
          re8.exec("FrffvbaQQS2=9995p6rp12rrnr893334ro7nq70o7p64p69rqn844prs1473");
          re8.exec("__hgzn=144631658.2770915348920628700.1231367708.1231367708.1231367708.1");
          re8.exec("__hgzo=144631658.0.10.1231367708");
          re8.exec("__hgzm=144631658.1231367708.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re34.exec(s99(i));
          re34.exec(s100(i));
          """/ZFVR\s+5[.]01/""".R.exec(s15(i));
          """/HF(?=;)/i""".R.exec(str56);
          re74.exec(s97(i));
          re28.exec("svefg npgvir svefgNpgvir");
          re28.exec("ynfg");
          """/\bp:(..)/i""".R.exec("m:94043|yn:37.4154|yb:-122.0585|p:HF");
          re75.exec(str57);
          re75.exec(str58);
          re76.exec(str57);
          re76.exec(str58);
          re77.exec(str57);
          re77.exec(str58);
          """/\bhfucce\s*=\s*([^;]*)/i""".R.exec(str59);
          re78.exec(str57);
          re78.exec(str58);
          """/\bjci\s*=\s*([^;]*)/i""".R.exec(str59);
          re79.exec(str58);
          re79.exec(str60);
          re79.exec(str59);
          """/\|p:([a-z]{2})/i""".R.exec("m:94043|yn:37.4154|yb:-122.0585|p:HF|ue:1");
          re80.exec(s97(i));
          re61.exec("cebgbglcr.wf");
          re68.exec(s97(i));
          re81.exec(s97(i));
          re82.exec(s97(i));
          """/^Fubpxjnir Synfu (\d)/""".R.exec(s21(i));
          """/^Fubpxjnir Synfu (\d+)/""".R.exec(s21(i));
          re83.exec("[bowrpg tybony]");
          re62.exec(s97(i));
          re84.exec(str61);
          re84.exec(str62);
          """/jroxvg/""".R.exec(str63);
        }
      }
      var re85 = """/eaq_zbqobkva/""".R;
      var str64 = "1231365729213";
      var str65 = "74.125.75.3-1057165600.29978900";
      var str66 = "74.125.75.3-1057165600.29978900.1231365730214";
      var str67 = "Frnepu%20Zvpebfbsg.pbz";
      var str68 = "FrffvbaQQS2=8sqq78r9n442851q565599o401385sp3s04r92rnn7o19ssn; ZFPhygher=VC=74.125.75.17&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669340386893867&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var str69 = "FrffvbaQQS2=8sqq78r9n442851q565599o401385sp3s04r92rnn7o19ssn; __hgzm=144631658.1231365779.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar); __hgzn=144631658.1877536177953918500.1231365779.1231365779.1231365779.1; __hgzo=144631658.0.10.1231365779; __hgzp=144631658; ZFPhygher=VC=74.125.75.17&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669340386893867&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str70 = "I=3%26THVQ=757q3ss871q44o7o805n8113n5p72q52";
      var str71 = "I=3&THVQ=757q3ss871q44o7o805n8113n5p72q52";
      var str72 = "uggc://tbbtyrnqf.t.qbhoyrpyvpx.arg/cntrnq/nqf?pyvrag=pn-svz_zlfcnpr_zlfcnpr-ubzrcntr_wf&qg=1231365765292&uy=ra&nqfnsr=uvtu&br=hgs8&ahz_nqf=4&bhgchg=wf&nqgrfg=bss&pbeeryngbe=1231365765292&punaary=svz_zlfcnpr_ubzrcntr_abgybttrqva%2Psvz_zlfcnpr_aba_HTP%2Psvz_zlfcnpr_havgrq-fgngrf&hey=uggc%3N%2S%2Sohyyrgvaf.zlfcnpr.pbz%2Svaqrk.psz&nq_glcr=grkg&rvq=6083027&rn=0&sez=0&tn_ivq=1579793869.1231365768&tn_fvq=1231365768&tn_uvq=2056210897&synfu=9.0.115&h_u=768&h_j=1024&h_nu=738&h_nj=1024&h_pq=24&h_gm=-480&h_uvf=2&h_wnin=gehr&h_acyht=7&h_azvzr=22";
      var str73 = "frnepu.zvpebfbsg.pbz";
      var str74 = "frnepu.zvpebfbsg.pbz/";
      var str75 = "ZFPhygher=VC=74.125.75.17&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669340386893867&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str76 = "ZFPhygher=VC=74.125.75.17&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669340386893867&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      def runBlock10() {
        for (i <- 0 until 3) {
          "%3Szxg=ra-HF".replace(re39, "");
          "-8".replace(re40, "");
          "-8".replace(re10, "");
          "-8".replace(re51, "");
          "-8".replace(re52, "");
          "-8".replace(re53, "");
          "-8".replace(re39, "");
          "-8".replace(re54, "");
          "1.5".replace(re40, "");
          "1.5".replace(re10, "");
          "1.5".replace(re51, "");
          "1.5".replace(re52, "");
          "1.5".replace(re53, "");
          "1.5".replace(re39, "");
          "1.5".replace(re54, "");
          "1024k768".replace(re40, "");
          "1024k768".replace(re10, "");
          "1024k768".replace(re51, "");
          "1024k768".replace(re52, "");
          "1024k768".replace(re53, "");
          "1024k768".replace(re39, "");
          "1024k768".replace(re54, "");
          str64.replace(re40, "");
          str64.replace(re10, "");
          str64.replace(re51, "");
          str64.replace(re52, "");
          str64.replace(re53, "");
          str64.replace(re39, "");
          str64.replace(re54, "");
          "14".replace(re40, "");
          "14".replace(re10, "");
          "14".replace(re51, "");
          "14".replace(re52, "");
          "14".replace(re53, "");
          "14".replace(re39, "");
          "14".replace(re54, "");
          "24".replace(re40, "");
          "24".replace(re10, "");
          "24".replace(re51, "");
          "24".replace(re52, "");
          "24".replace(re53, "");
          "24".replace(re39, "");
          "24".replace(re54, "");
          str65.replace(re40, "");
          str65.replace(re10, "");
          str65.replace(re51, "");
          str65.replace(re52, "");
          str65.replace(re53, "");
          str65.replace(re39, "");
          str65.replace(re54, "");
          str66.replace(re40, "");
          str66.replace(re10, "");
          str66.replace(re51, "");
          str66.replace(re52, "");
          str66.replace(re53, "");
          str66.replace(re39, "");
          str66.replace(re54, "");
          "9.0".replace(re40, "");
          "9.0".replace(re10, "");
          "9.0".replace(re51, "");
          "9.0".replace(re52, "");
          "9.0".replace(re53, "");
          "9.0".replace(re39, "");
          "9.0".replace(re54, "");
          "994k634".replace(re40, "");
          "994k634".replace(re10, "");
          "994k634".replace(re51, "");
          "994k634".replace(re52, "");
          "994k634".replace(re53, "");
          "994k634".replace(re39, "");
          "994k634".replace(re54, "");
          "?zxg=ra-HF".replace(re40, "");
          "?zxg=ra-HF".replace(re10, "");
          "?zxg=ra-HF".replace(re51, "");
          "?zxg=ra-HF".replace(re52, "");
          "?zxg=ra-HF".replace(re53, "");
          "?zxg=ra-HF".replace(re54, "");
          "PAA.pbz".replace(re25, "");
          "PAA.pbz".replace(re12, "");
          "PAA.pbz".replace(re39, "");
          "Qngr & Gvzr".replace(re25, "");
          "Qngr & Gvzr".replace(re12, "");
          "Qngr & Gvzr".replace(re39, "");
          "Frnepu Zvpebfbsg.pbz".replace(re40, "");
          "Frnepu Zvpebfbsg.pbz".replace(re54, "");
          str67.replace(re10, "");
          str67.replace(re51, "");
          str67.replace(re52, "");
          str67.replace(re53, "");
          str67.replace(re39, "");
          str68.split(re32);
          str69.split(re32);
          str70.replace(re52, "");
          str70.replace(re53, "");
          str70.replace(re39, "");
          str71.replace(re40, "");
          str71.replace(re10, "");
          str71.replace(re51, "");
          str71.replace(re54, "");
          "Jrngure".replace(re25, "");
          "Jrngure".replace(re12, "");
          "Jrngure".replace(re39, "");
          "LbhGhor".replace(re25, "");
          "LbhGhor".replace(re12, "");
          "LbhGhor".replace(re39, "");
          str72.replace(re33, "");
          "erzbgr_vsenzr_1".replace("""/^erzbgr_vsenzr_/""".R, "");
          str73.replace(re40, "");
          str73.replace(re10, "");
          str73.replace(re51, "");
          str73.replace(re52, "");
          str73.replace(re53, "");
          str73.replace(re39, "");
          str73.replace(re54, "");
          str74.replace(re40, "");
          str74.replace(re10, "");
          str74.replace(re51, "");
          str74.replace(re52, "");
          str74.replace(re53, "");
          str74.replace(re39, "");
          str74.replace(re54, "");
          "lhv-h".replace("""/\-/g""".R, "");
          re9.exec("p");
          re9.exec("qz p");
          re9.exec("zbqynory");
          re9.exec("lhv-h svefg");
          re8.exec("144631658.0.10.1231365779");
          re8.exec("144631658.1231365779.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re8.exec("144631658.1877536177953918500.1231365779.1231365779.1231365779.1");
          re8.exec(str75);
          re8.exec(str76);
          re8.exec("__hgzn=144631658.1877536177953918500.1231365779.1231365779.1231365779.1");
          re8.exec("__hgzo=144631658.0.10.1231365779");
          re8.exec("__hgzm=144631658.1231365779.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re34.exec(str68);
          re34.exec(str69);
          """/^$/""".R.exec("");
          re31.exec("qr");
          """/^znk\d+$/""".R.exec("");
          """/^zva\d+$/""".R.exec("");
          """/^erfgber$/""".R.exec("");
          re85.exec("zbqobkva zbqobk_abcnqqvat ");
          re85.exec("zbqgvgyr");
          re85.exec("eaq_zbqobkva ");
          re85.exec("eaq_zbqgvgyr ");
          """/frpgvba\d+_pbagragf/""".R.exec("obggbz_ani");
        }
      }
      var re86 = """/;\s*/""".R;
      var re87 = """/(\$\{inyhr\})|(\$inyhr\b)/g""".R;
      var re88 = """/(\$\{abj\})|(\$abj\b)/g""".R;
      var re89 = """/\s+$/""".R;
      var re90 = """/^\s+/""".R;
      var re91 = """/(\\\"|\x00-|\x1f|\x7f-|\x9f|\u005cu00ad|\u005cu0600-|\u005cu0604|\u005cu070f|\u005cu17b4|\u005cu17b5|\u005cu200c-|\u005cu200f|\u005cu2028-|\u005cu202f|\u005cu2060-|\u005cu206f|\u005cufeff|\u005cufff0-|\u005cuffff)/g""".R;
      var re92 = """/^(:)([\w-]+)\("?'?(.*?(\(.*?\))?[^(]*?)"?'?\)/""".R;
      var re93 = """/^([:.#]*)((?:[\w\u005cu0128-\u005cuffff*_-]|\\.)+)/""".R;
      var re94 = """/^(\[) *@?([\w-]+) *([!*$^~=]*) *('?"?)(.*?)\4 *\]/""".R;
      var str77 = "#fubhgobk .pybfr";
      var str78 = "FrffvbaQQS2=102n9o0o9pq60132qn0337rr867p75953502q2s27s2s5r98; ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669341278771470&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=; AFP_zp_dfctwzssrwh-aowb_80=441326q33660";
      var str79 = "FrffvbaQQS2=102n9o0o9pq60132qn0337rr867p75953502q2s27s2s5r98; AFP_zp_dfctwzssrwh-aowb_80=441326q33660; __hgzm=144631658.1231365869.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar); __hgzn=144631658.1670816052019209000.1231365869.1231365869.1231365869.1; __hgzo=144631658.0.10.1231365869; __hgzp=144631658; ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669341278771470&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str80 = "FrffvbaQQS2=9995p6rp12rrnr893334ro7nq70o7p64p69rqn844prs1473; ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669350559478880&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=; AFP_zp_dfctwzs-aowb_80=441327q73660";
      var str81 = "FrffvbaQQS2=9995p6rp12rrnr893334ro7nq70o7p64p69rqn844prs1473; AFP_zp_dfctwzs-aowb_80=441327q73660; __hgzm=144631658.1231367054.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar); __hgzn=144631658.1796080716621419500.1231367054.1231367054.1231367054.1; __hgzo=144631658.0.10.1231367054; __hgzp=144631658; ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669350559478880&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str82 = "[glcr=fhozvg]";
      var str83 = "n.svryqOga,n.svryqOgaPnapry";
      var str84 = "n.svryqOgaPnapry";
      var str85 = "oyvpxchaxg";
      var str86 = "qvi.bow-nppbeqvba qg";
      var str87 = "uggc://tbbtyrnqf.t.qbhoyrpyvpx.arg/cntrnq/nqf?pyvrag=pn-svz_zlfcnpr_nccf_wf&qg=1231367052227&uy=ra&nqfnsr=uvtu&br=hgs8&ahz_nqf=4&bhgchg=wf&nqgrfg=bss&pbeeryngbe=1231367052227&punaary=svz_zlfcnpr_nccf-pnainf%2Psvz_zlfcnpr_havgrq-fgngrf&hey=uggc%3N%2S%2Scebsvyr.zlfcnpr.pbz%2SZbqhyrf%2SNccyvpngvbaf%2SCntrf%2SPnainf.nfck&nq_glcr=grkg&rvq=6083027&rn=0&sez=1&tn_ivq=716357910.1231367056&tn_fvq=1231367056&tn_uvq=1387206491&synfu=9.0.115&h_u=768&h_j=1024&h_nu=738&h_nj=1024&h_pq=24&h_gm=-480&h_uvf=2&h_wnin=gehr&h_acyht=7&h_azvzr=22";
      var str88 = "uggc://tbbtyrnqf.t.qbhoyrpyvpx.arg/cntrnq/nqf?pyvrag=pn-svz_zlfcnpr_zlfcnpr-ubzrcntr_wf&qg=1231365851658&uy=ra&nqfnsr=uvtu&br=hgs8&ahz_nqf=4&bhgchg=wf&nqgrfg=bss&pbeeryngbe=1231365851658&punaary=svz_zlfcnpr_ubzrcntr_abgybttrqva%2Psvz_zlfcnpr_aba_HTP%2Psvz_zlfcnpr_havgrq-fgngrf&hey=uggc%3N%2S%2Scebsvyrrqvg.zlfcnpr.pbz%2Svaqrk.psz&nq_glcr=grkg&rvq=6083027&rn=0&sez=0&tn_ivq=1979828129.1231365855&tn_fvq=1231365855&tn_uvq=2085229649&synfu=9.0.115&h_u=768&h_j=1024&h_nu=738&h_nj=1024&h_pq=24&h_gm=-480&h_uvf=2&h_wnin=gehr&h_acyht=7&h_azvzr=22";
      var str89 = "uggc://zfacbegny.112.2b7.arg/o/ff/zfacbegnyubzr/1/U.7-cqi-2/f55023338617756?[NDO]&aqu=1&g=7%2S0%2S2009%2014%3N12%3N47%203%20480&af=zfacbegny&cntrAnzr=HF%20UCZFSGJ&t=uggc%3N%2S%2Sjjj.zfa.pbz%2S&f=0k0&p=43835816&x=A&oj=994&ou=634&uc=A&{2}&[NDR]";
      var str90 = "zrgn[anzr=nwnkHey]";
      var str91 = "anpuevpugra";
/*ESC*/      var str92 = """b oS={'oT':1.1};x $8n(B){z(B!=o9)};x $S(B){O(!$8n(B))z A;O(B.4L)z'T';b S=7t B;O(S=='2P'&&B.p4){23(B.7f){12 1:z'T';12 3:z/S/.2g(B.8M)?'ox':'oh'}}O(S=='2P'||S=='x'){23(B.nE){12 2V:z'1O';12 7I:z'5a';12 18:z'4B'}O(7t B.I=='4F'){O(B.3u)z'pG';O(B.8e)z'1p'}}z S};x $2p(){b 4E={};Z(b v=0;v<1p.I;v++){Z(b X 1o 1p[v]){b nc=1p[v][X];b 6E=4E[X];O(6E&&$S(nc)=='2P'&&$S(6E)=='2P')4E[X]=$2p(6E,nc);17 4E[X]=nc}}z 4E};b $E=7p.E=x(){b 1d=1p;O(!1d[1])1d=[p,1d[0]];Z(b X 1o 1d[1])1d[0][X]=1d[1][X];z 1d[0]};b $4D=7p.pJ=x(){Z(b v=0,y=1p.I;v<y;v++){1p[v].E=x(1J){Z(b 1I 1o 1J){O(!p.1Y[1I])p.1Y[1I]=1J[1I];O(!p[1I])p[1I]=$4D.6C(1I)}}}};$4D.6C=x(1I){z x(L){z p.1Y[1I].3H(L,2V.1Y.nV.1F(1p,1))}};$4D(7F,2V,6J,nb);b 3l=x(B){B=B||{};B.E=$E;z B};b pK=Y 3l(H);b pZ=Y 3l(C);C.6f=C.35('6f')[0];x $2O(B){z!!(B||B===0)};x $5S(B,n8){z $8n(B)?B:n8};x $7K(3c,1m){z 1q.na(1q.7K()*(1m-3c+1)+3c)};x $3N(){z Y 97().os()};x $4M(1U){pv(1U);pa(1U);z 1S};H.43=!!(C.5Z);O(H.nB)H.31=H[H.7q?'py':'nL']=1r;17 O(C.9N&&!C.om&&!oy.oZ)H.pF=H.4Z=H[H.43?'pt':'65']=1r;17 O(C.po!=1S)H.7J=1r;O(7t 5B=='o9'){b 5B=x(){};O(H.4Z)C.nd("pW");5B.1Y=(H.4Z)?H["[[oN.1Y]]"]:{}}5B.1Y.4L=1r;O(H.nL)5s{C.oX("pp",A,1r)}4K(r){};b 18=x(1X){b 63=x(){z(1p[0]!==1S&&p.1w&&$S(p.1w)=='x')?p.1w.3H(p,1p):p};$E(63,p);63.1Y=1X;63.nE=18;z 63};18.1z=x(){};18.1Y={E:x(1X){b 7x=Y p(1S);Z(b X 1o 1X){b nC=7x[X];7x[X]=18.nY(nC,1X[X])}z Y 18(7x)},3d:x(){Z(b v=0,y=1p.I;v<y;v++)$E(p.1Y,1p[v])}};18.nY=x(2b,2n){O(2b&&2b!=2n){b S=$S(2n);O(S!=$S(2b))z 2n;23(S){12'x':b 7R=x(){p.1e=1p.8e.1e;z 2n.3H(p,1p)};7R.1e=2b;z 7R;12'2P':z $2p(2b,2n)}}z 2n};b 8o=Y 18({oQ:x(J){p.4w=p.4w||[];p.4w.1x(J);z p},7g:x(){O(p.4w&&p.4w.I)p.4w.9J().2x(10,p)},oP:x(){p.4w=[]}});b 2d=Y 18({1V:x(S,J){O(J!=18.1z){p.$19=p.$19||{};p.$19[S]=p.$19[S]||[];p.$19[S].5j(J)}z p},1v:x(S,1d,2x){O(p.$19&&p.$19[S]){p.$19[S].1b(x(J){J.3n({'L':p,'2x':2x,'1p':1d})()},p)}z p},3M:x(S,J){O(p.$19&&p.$19[S])p.$19[S].2U(J);z p}});b 4v=Y 18({2H:x(){p.P=$2p.3H(1S,[p.P].E(1p));O(!p.1V)z p;Z(b 3O 1o p.P){O($S(p.P[3O]=='x')&&3O.2g(/^5P[N-M]/))p.1V(3O,p.P[3O])}z p}});2V.E({7y:x(J,L){Z(b v=0,w=p.I;v<w;v++)J.1F(L,p[v],v,p)},3s:x(J,L){b 54=[];Z(b v=0,w=p.I;v<w;v++){O(J.1F(L,p[v],v,p))54.1x(p[v])}z 54},2X:x(J,L){b 54=[];Z(b v=0,w=p.I;v<w;v++)54[v]=J.1F(L,p[v],v,p);z 54},4i:x(J,L){Z(b v=0,w=p.I;v<w;v++){O(!J.1F(L,p[v],v,p))z A}z 1r},ob:x(J,L){Z(b v=0,w=p.I;v<w;v++){O(J.1F(L,p[v],v,p))z 1r}z A},3F:x(3u,15){b 3A=p.I;Z(b v=(15<0)?1q.1m(0,3A+15):15||0;v<3A;v++){O(p[v]===3u)z v}z-1},8z:x(1u,I){1u=1u||0;O(1u<0)1u=p.I+1u;I=I||(p.I-1u);b 89=[];Z(b v=0;v<I;v++)89[v]=p[1u++];z 89},2U:x(3u){b v=0;b 3A=p.I;6L(v<3A){O(p[v]===3u){p.6l(v,1);3A--}17{v++}}z p},1y:x(3u,15){z p.3F(3u,15)!=-1},oz:x(1C){b B={},I=1q.3c(p.I,1C.I);Z(b v=0;v<I;v++)B[1C[v]]=p[v];z B},E:x(1O){Z(b v=0,w=1O.I;v<w;v++)p.1x(1O[v]);z p},2p:x(1O){Z(b v=0,y=1O.I;v<y;v++)p.5j(1O[v]);z p},5j:x(3u){O(!p.1y(3u))p.1x(3u);z p},oc:x(){z p[$7K(0,p.I-1)]||A},7L:x(){z p[p.I-1]||A}});2V.1Y.1b=2V.1Y.7y;2V.1Y.2g=2V.1Y.1y;x $N(1O){z 2V.8z(1O)};x $1b(3J,J,L){O(3J&&7t 3J.I=='4F'&&$S(3J)!='2P')2V.7y(3J,J,L);17 Z(b 1j 1o 3J)J.1F(L||3J,3J[1j],1j)};6J.E({2g:x(6b,2F){z(($S(6b)=='2R')?Y 7I(6b,2F):6b).2g(p)},3p:x(){z 5K(p,10)},o4:x(){z 69(p)},7A:x(){z p.3y(/-D/t,x(2G){z 2G.7G(1).nW()})},9b:x(){z p.3y(/w[N-M]/t,x(2G){z(2G.7G(0)+'-'+2G.7G(1).5O())})},8V:x(){z p.3y(/\b[n-m]/t,x(2G){z 2G.nW()})},5L:x(){z p.3y(/^s+|s+$/t,'')},7j:x(){z p.3y(/s{2,}/t,' ').5L()},5V:x(1O){b 1i=p.2G(/d{1,3}/t);z(1i)?1i.5V(1O):A},5U:x(1O){b 3P=p.2G(/^#?(w{1,2})(w{1,2})(w{1,2})$/);z(3P)?3P.nV(1).5U(1O):A},1y:x(2R,f){z(f)?(f+p+f).3F(f+2R+f)>-1:p.3F(2R)>-1},nX:x(){z p.3y(/([.*+?^${}()|[]/\])/t,'\$1')}});2V.E({5V:x(1O){O(p.I<3)z A;O(p.I==4&&p[3]==0&&!1O)z'p5';b 3P=[];Z(b v=0;v<3;v++){b 52=(p[v]-0).4h(16);3P.1x((52.I==1)?'0'+52:52)}z 1O?3P:'#'+3P.2u('')},5U:x(1O){O(p.I!=3)z A;b 1i=[];Z(b v=0;v<3;v++){1i.1x(5K((p[v].I==1)?p[v]+p[v]:p[v],16))}z 1O?1i:'1i('+1i.2u(',')+')'}});7F.E({3n:x(P){b J=p;P=$2p({'L':J,'V':A,'1p':1S,'2x':A,'4s':A,'6W':A},P);O($2O(P.1p)&&$S(P.1p)!='1O')P.1p=[P.1p];z x(V){b 1d;O(P.V){V=V||H.V;1d=[(P.V===1r)?V:Y P.V(V)];O(P.1p)1d.E(P.1p)}17 1d=P.1p||1p;b 3C=x(){z J.3H($5S(P""";
      var str93 = "hagreunyghat";
      var str94 = "ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669341278771470&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str95 = "ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&Pbhagel=IIZ%3Q&SbeprqRkcvengvba=633669350559478880&gvzrMbar=-8&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R%3Q";
      var str96 = "ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669341278771470&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var str97 = "ZFPhygher=VC=74.125.75.1&VCPhygher=ra-HF&CersreerqPhygher=ra-HF&CersreerqPhygherCraqvat=&Pbhagel=IIZ=&SbeprqRkcvengvba=633669350559478880&gvzrMbar=0&HFEYBP=DKWyLHAiMTH9AwHjWxAcqUx9GJ91oaEunJ4tIzyyqlMQo3IhqUW5D29xMG1IHlMQo3IhqUW5GzSgMG1Iozy0MJDtH3EuqTImWxEgLHAiMTH9BQN3WxkuqTy0qJEyCGZ3YwDkBGVzGT9hM2y0qJEyCF0kZwVhZQH3APMDo3A0LJkQo2EyCGx0ZQDmWyWyM2yiox5uoJH9D0R=";
      var str98 = "shapgvba (){Cuk.Nccyvpngvba.Frghc.Pber();Cuk.Nccyvpngvba.Frghc.Nwnk();Cuk.Nccyvpngvba.Frghc.Synfu();Cuk.Nccyvpngvba.Frghc.Zbqhyrf()}";


      // TR: hitting max allowed method args (66?), needed to split
      
      var runBlock11A = () => ()
      var runBlock11B = () => ()

      def runBlock11() {
        for (i <- 0 until 2) {
          runBlock11A()
          runBlock11B()
        }
      }

      runBlock11A = { () =>
          " .pybfr".replace(re18, "");
          " n.svryqOgaPnapry".replace(re18, "");
          " qg".replace(re18, "");
          str77.replace(re68, "");
          str77.replace(re18, "");
          "".replace(re39, "");
          "".replace("""/^/""".R, "");
          "".split(re86);
          "*".replace(re39, "");
          "*".replace(re68, "");
          "*".replace(re18, "");
          ".pybfr".replace(re68, "");
          ".pybfr".replace(re18, "");
          "//vzt.jro.qr/vij/FC/tzk_uc/fperra/${inyhr}?gf=${abj}".replace(re87, "");
          "//vzt.jro.qr/vij/FC/tzk_uc/fperra/1024?gf=${abj}".replace(re88, "");
          "//vzt.jro.qr/vij/FC/tzk_uc/jvafvmr/${inyhr}?gf=${abj}".replace(re87, "");
          "//vzt.jro.qr/vij/FC/tzk_uc/jvafvmr/992/608?gf=${abj}".replace(re88, "");
          "300k120".replace(re30, "");
          "300k250".replace(re30, "");
          "310k120".replace(re30, "");
          "310k170".replace(re30, "");
          "310k250".replace(re30, "");
          "9.0  e115".replace("""/^.*\.(.*)\s.*$/""".R, "");
          "Nppbeqvba".replace(re2, "");
          "Nxghryy\n".replace(re89, "");
          "Nxghryy\n".replace(re90, "");
          "Nccyvpngvba".replace(re2, "");
          "Oyvpxchaxg\n".replace(re89, "");
          "Oyvpxchaxg\n".replace(re90, "");
          "Svanamra\n".replace(re89, "");
          "Svanamra\n".replace(re90, "");
          "Tnzrf\n".replace(re89, "");
          "Tnzrf\n".replace(re90, "");
          "Ubebfxbc\n".replace(re89, "");
          "Ubebfxbc\n".replace(re90, "");
          "Xvab\n".replace(re89, "");
          "Xvab\n".replace(re90, "");
          "Zbqhyrf".replace(re2, "");
          "Zhfvx\n".replace(re89, "");
          "Zhfvx\n".replace(re90, "");
          "Anpuevpugra\n".replace(re89, "");
          "Anpuevpugra\n".replace(re90, "");
          "Cuk".replace(re2, "");
          "ErdhrfgSvavfu".split(re70);
          "ErdhrfgSvavfu.NWNK.Cuk".split(re70);
          "Ebhgr\n".replace(re89, "");
          "Ebhgr\n".replace(re90, "");
          str78.split(re32);
          str79.split(re32);
          str80.split(re32);
          str81.split(re32);
          "Fcbeg\n".replace(re89, "");
          "Fcbeg\n".replace(re90, "");
          "GI-Fcbg\n".replace(re89, "");
          "GI-Fcbg\n".replace(re90, "");
          "Gbhe\n".replace(re89, "");
          "Gbhe\n".replace(re90, "");
          "Hagreunyghat\n".replace(re89, "");
          "Hagreunyghat\n".replace(re90, "");
          "Ivqrb\n".replace(re89, "");
          "Ivqrb\n".replace(re90, "");
          "Jrggre\n".replace(re89, "");
          "Jrggre\n".replace(re90, "");
          str82.replace(re68, "");
          str82.replace(re18, "");
          str83.replace(re68, "");
          str83.replace(re18, "");
          str84.replace(re68, "");
          str84.replace(re18, "");
          "nqiFreivprObk".replace(re30, "");
          "nqiFubccvatObk".replace(re30, "");
          "nwnk".replace(re39, "");
          "nxghryy".replace(re40, "");
          "nxghryy".replace(re41, "");
          "nxghryy".replace(re42, "");
          "nxghryy".replace(re43, "");
          "nxghryy".replace(re44, "");
          "nxghryy".replace(re45, "");
          "nxghryy".replace(re46, "");
          "nxghryy".replace(re47, "");
          "nxghryy".replace(re48, "");
          str85.replace(re40, "");
          str85.replace(re41, "");
          str85.replace(re42, "");
          str85.replace(re43, "");
          str85.replace(re44, "");
          str85.replace(re45, "");
          str85.replace(re46, "");
          str85.replace(re47, "");
          str85.replace(re48, "");
          "pngrtbel".replace(re29, "");
          "pngrtbel".replace(re30, "");
          "pybfr".replace(re39, "");
          "qvi".replace(re39, "");
          str86.replace(re68, "");
          str86.replace(re18, "");
          "qg".replace(re39, "");
          "qg".replace(re68, "");
          "qg".replace(re18, "");
          "rzorq".replace(re39, "");
          "rzorq".replace(re68, "");
          "rzorq".replace(re18, "");
          "svryqOga".replace(re39, "");
          "svryqOgaPnapry".replace(re39, "");
          "svz_zlfcnpr_nccf-pnainf,svz_zlfcnpr_havgrq-fgngrf".split(re20);
          "svanamra".replace(re40, "");
          "svanamra".replace(re41, "");
          "svanamra".replace(re42, "");
          "svanamra".replace(re43, "");
          "svanamra".replace(re44, "");
          "svanamra".replace(re45, "");
          "svanamra".replace(re46, "");
          "svanamra".replace(re47, "");
          "svanamra".replace(re48, "");
          "sbphf".split(re70);
          "sbphf.gno sbphfva.gno".split(re70);
          "sbphfva".split(re70);
          "sbez".replace(re39, "");
          "sbez.nwnk".replace(re68, "");
          "sbez.nwnk".replace(re18, "");
          "tnzrf".replace(re40, "");
          "tnzrf".replace(re41, "");
          "tnzrf".replace(re42, "");
          "tnzrf".replace(re43, "");
          "tnzrf".replace(re44, "");
          "tnzrf".replace(re45, "");
          "tnzrf".replace(re46, "");
          "tnzrf".replace(re47, "");
          "tnzrf".replace(re48, "");
          "ubzrcntr".replace(re30, "");
          "ubebfxbc".replace(re40, "");
          "ubebfxbc".replace(re41, "");
          "ubebfxbc".replace(re42, "");
          "ubebfxbc".replace(re43, "");
          "ubebfxbc".replace(re44, "");
          "ubebfxbc".replace(re45, "");
          "ubebfxbc".replace(re46, "");
          "ubebfxbc".replace(re47, "");
          "ubebfxbc".replace(re48, "");
          "uc_cebzbobk_ugzy%2Puc_cebzbobk_vzt".replace(re30, "");
          "uc_erpgnatyr".replace(re30, "");
          str87.replace(re33, "");
          str88.replace(re33, "");
          "uggc://wf.hv-cbegny.qr/tzk/ubzr/wf/20080602/onfr.wf${4}${5}".replace(re71, "");
          "uggc://wf.hv-cbegny.qr/tzk/ubzr/wf/20080602/onfr.wf${5}".replace(re72, "");
          "uggc://wf.hv-cbegny.qr/tzk/ubzr/wf/20080602/qlaYvo.wf${4}${5}".replace(re71, "");
          "uggc://wf.hv-cbegny.qr/tzk/ubzr/wf/20080602/qlaYvo.wf${5}".replace(re72, "");
          "uggc://wf.hv-cbegny.qr/tzk/ubzr/wf/20080602/rssrpgYvo.wf${4}${5}".replace(re71, "");
          "uggc://wf.hv-cbegny.qr/tzk/ubzr/wf/20080602/rssrpgYvo.wf${5}".replace(re72, "");
          str89.replace(re73, "");
          "uggc://zfacbegny.112.2b7.arg/o/ff/zfacbegnyubzr/1/U.7-cqi-2/f55023338617756?[NDO]&{1}&{2}&[NDR]".replace(re69, "");
          str6.replace(re23, "");
          "xvab".replace(re40, "");
          "xvab".replace(re41, "");
          "xvab".replace(re42, "");
          "xvab".replace(re43, "");
          "xvab".replace(re44, "");
          "xvab".replace(re45, "");
          "xvab".replace(re46, "");
          "xvab".replace(re47, "");
          "xvab".replace(re48, "");
          "ybnq".split(re70);
          "zrqvnzbqgno lhv-anifrg lhv-anifrg-gbc".replace(re18, "");
          "zrgn".replace(re39, "");
          str90.replace(re68, "");
          str90.replace(re18, "");
          "zbhfrzbir".split(re70);
          "zbhfrzbir.gno".split(re70);
          str63.replace("""/^.*jroxvg\/(\d+(\.\d+)?).*$/""".R, "");
          "zhfvx".replace(re40, "");
          "zhfvx".replace(re41, "");
          "zhfvx".replace(re42, "");
          "zhfvx".replace(re43, "");
          "zhfvx".replace(re44, "");
          "zhfvx".replace(re45, "");
          "zhfvx".replace(re46, "");
          "zhfvx".replace(re47, "");
          "zhfvx".replace(re48, "");
          "zlfcnpr_nccf_pnainf".replace(re52, "");
          
        }

        runBlock11B = { () =>
          
          str91.replace(re40, "");
          str91.replace(re41, "");
          str91.replace(re42, "");
          str91.replace(re43, "");
          str91.replace(re44, "");
          str91.replace(re45, "");
          str91.replace(re46, "");
          str91.replace(re47, "");
          str91.replace(re48, "");
          "anzr".replace(re39, "");
          str92.replace("""/\b\w+\b/g""".R, "");
          "bow-nppbeqvba".replace(re39, "");
          "bowrpg".replace(re39, "");
          "bowrpg".replace(re68, "");
          "bowrpg".replace(re18, "");
          "cnenzf%2Rfglyrf".replace(re29, "");
          "cnenzf%2Rfglyrf".replace(re30, "");
          "cbchc".replace(re30, "");
          "ebhgr".replace(re40, "");
          "ebhgr".replace(re41, "");
          "ebhgr".replace(re42, "");
          "ebhgr".replace(re43, "");
          "ebhgr".replace(re44, "");
          "ebhgr".replace(re45, "");
          "ebhgr".replace(re46, "");
          "ebhgr".replace(re47, "");
          "ebhgr".replace(re48, "");
          "freivprobk_uc".replace(re30, "");
          "fubccvatobk_uc".replace(re30, "");
          "fubhgobk".replace(re39, "");
          "fcbeg".replace(re40, "");
          "fcbeg".replace(re41, "");
          "fcbeg".replace(re42, "");
          "fcbeg".replace(re43, "");
          "fcbeg".replace(re44, "");
          "fcbeg".replace(re45, "");
          "fcbeg".replace(re46, "");
          "fcbeg".replace(re47, "");
          "fcbeg".replace(re48, "");
          "gbhe".replace(re40, "");
          "gbhe".replace(re41, "");
          "gbhe".replace(re42, "");
          "gbhe".replace(re43, "");
          "gbhe".replace(re44, "");
          "gbhe".replace(re45, "");
          "gbhe".replace(re46, "");
          "gbhe".replace(re47, "");
          "gbhe".replace(re48, "");
          "gi-fcbg".replace(re40, "");
          "gi-fcbg".replace(re41, "");
          "gi-fcbg".replace(re42, "");
          "gi-fcbg".replace(re43, "");
          "gi-fcbg".replace(re44, "");
          "gi-fcbg".replace(re45, "");
          "gi-fcbg".replace(re46, "");
          "gi-fcbg".replace(re47, "");
          "gi-fcbg".replace(re48, "");
          "glcr".replace(re39, "");
          "haqrsvarq".replace("""/\//g""".R, "");
          str93.replace(re40, "");
          str93.replace(re41, "");
          str93.replace(re42, "");
          str93.replace(re43, "");
          str93.replace(re44, "");
          str93.replace(re45, "");
          str93.replace(re46, "");
          str93.replace(re47, "");
          str93.replace(re48, "");
          "ivqrb".replace(re40, "");
          "ivqrb".replace(re41, "");
          "ivqrb".replace(re42, "");
          "ivqrb".replace(re43, "");
          "ivqrb".replace(re44, "");
          "ivqrb".replace(re45, "");
          "ivqrb".replace(re46, "");
          "ivqrb".replace(re47, "");
          "ivqrb".replace(re48, "");
          "ivfvgf=1".split(re86);
          "jrggre".replace(re40, "");
          "jrggre".replace(re41, "");
          "jrggre".replace(re42, "");
          "jrggre".replace(re43, "");
          "jrggre".replace(re44, "");
          "jrggre".replace(re45, "");
          "jrggre".replace(re46, "");
          "jrggre".replace(re47, "");
          "jrggre".replace(re48, "");
          """/#[a-z0-9]+$/i""".R.exec("uggc://jjj.fpuhryreim.arg/Qrsnhyg");
          re66.exec("fryrpgrq");
          """/(?:^|\s+)lhv-ani(?:\s+|$)/""".R.exec("sff lhv-ani");
          """/(?:^|\s+)lhv-anifrg(?:\s+|$)/""".R.exec("zrqvnzbqgno lhv-anifrg");
          """/(?:^|\s+)lhv-anifrg-gbc(?:\s+|$)/""".R.exec("zrqvnzbqgno lhv-anifrg");
          re91.exec("GnoThvq");
          re91.exec("thvq");
          """/(pbzcngvoyr|jroxvg)/""".R.exec(str63);
          """/.+(?:ei|vg|en|vr)[\/: ]([\d.]+)/""".R.exec(str63);
          re8.exec("144631658.0.10.1231365869");
          re8.exec("144631658.0.10.1231367054");
          re8.exec("144631658.1231365869.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re8.exec("144631658.1231367054.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re8.exec("144631658.1670816052019209000.1231365869.1231365869.1231365869.1");
          re8.exec("144631658.1796080716621419500.1231367054.1231367054.1231367054.1");
          re8.exec(str94);
          re8.exec(str95);
          re8.exec(str96);
          re8.exec(str97);
          re8.exec("__hgzn=144631658.1670816052019209000.1231365869.1231365869.1231365869.1");
          re8.exec("__hgzn=144631658.1796080716621419500.1231367054.1231367054.1231367054.1");
          re8.exec("__hgzo=144631658.0.10.1231365869");
          re8.exec("__hgzo=144631658.0.10.1231367054");
          re8.exec("__hgzm=144631658.1231365869.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re8.exec("__hgzm=144631658.1231367054.1.1.hgzpfe=(qverpg)|hgzppa=(qverpg)|hgzpzq=(abar)");
          re34.exec(str78);
          re34.exec(str79);
          re34.exec(str81);
          re74.exec(str77);
          re74.exec("*");
          re74.exec(str82);
          re74.exec(str83);
          re74.exec(str86);
          re74.exec("rzorq");
          re74.exec("sbez.nwnk");
          re74.exec(str90);
          re74.exec("bowrpg");
          """/\/onfr.wf(\?.+)?$/""".R.exec("/uggc://wf.hv-cbegny.qr/tzk/ubzr/wf/20080602/onfr.wf");
          re28.exec("uvag ynfgUvag ynfg");
          re75.exec("");
          re76.exec("");
          re77.exec("");
          re78.exec("");
          re80.exec(str77);
          re80.exec("*");
          re80.exec(".pybfr");
          re80.exec(str82);
          re80.exec(str83);
          re80.exec(str84);
          re80.exec(str86);
          re80.exec("qg");
          re80.exec("rzorq");
          re80.exec("sbez.nwnk");
          re80.exec(str90);
          re80.exec("bowrpg");
          re61.exec("qlaYvo.wf");
          re61.exec("rssrpgYvo.wf");
          re61.exec("uggc://jjj.tzk.arg/qr/?fgnghf=uvajrvf");
          re92.exec(" .pybfr");
          re92.exec(" n.svryqOgaPnapry");
          re92.exec(" qg");
          re92.exec(str48);
          re92.exec(".nwnk");
          re92.exec(".svryqOga,n.svryqOgaPnapry");
          re92.exec(".svryqOgaPnapry");
          re92.exec(".bow-nppbeqvba qg");
          re68.exec(str77);
          re68.exec("*");
          re68.exec(".pybfr");
          re68.exec(str82);
          re68.exec(str83);
          re68.exec(str84);
          re68.exec(str86);
          re68.exec("qg");
          re68.exec("rzorq");
          re68.exec("sbez.nwnk");
          re68.exec(str90);
          re68.exec("bowrpg");
          re93.exec(" .pybfr");
          re93.exec(" n.svryqOgaPnapry");
          re93.exec(" qg");
          re93.exec(str48);
          re93.exec(".nwnk");
          re93.exec(".svryqOga,n.svryqOgaPnapry");
          re93.exec(".svryqOgaPnapry");
          re93.exec(".bow-nppbeqvba qg");
          re81.exec(str77);
          re81.exec("*");
          re81.exec(str48);
          re81.exec(".pybfr");
          re81.exec(str82);
          re81.exec(str83);
          re81.exec(str84);
          re81.exec(str86);
          re81.exec("qg");
          re81.exec("rzorq");
          re81.exec("sbez.nwnk");
          re81.exec(str90);
          re81.exec("bowrpg");
          re94.exec(" .pybfr");
          re94.exec(" n.svryqOgaPnapry");
          re94.exec(" qg");
          re94.exec(str48);
          re94.exec(".nwnk");
          re94.exec(".svryqOga,n.svryqOgaPnapry");
          re94.exec(".svryqOgaPnapry");
          re94.exec(".bow-nppbeqvba qg");
          re94.exec("[anzr=nwnkHey]");
          re94.exec(str82);
          re31.exec("rf");
          re31.exec("wn");
          re82.exec(str77);
          re82.exec("*");
          re82.exec(str48);
          re82.exec(".pybfr");
          re82.exec(str82);
          re82.exec(str83);
          re82.exec(str84);
          re82.exec(str86);
          re82.exec("qg");
          re82.exec("rzorq");
          re82.exec("sbez.nwnk");
          re82.exec(str90);
          re82.exec("bowrpg");
          re83.exec(str98);
          re83.exec("shapgvba sbphf() { [angvir pbqr] }");
          re62.exec("#Ybtva");
          re62.exec("#Ybtva_cnffjbeq");
          re62.exec(str77);
          re62.exec("#fubhgobkWf");
          re62.exec("#fubhgobkWfReebe");
          re62.exec("#fubhgobkWfFhpprff");
          re62.exec("*");
          re62.exec(str82);
          re62.exec(str83);
          re62.exec(str86);
          re62.exec("rzorq");
          re62.exec("sbez.nwnk");
          re62.exec(str90);
          re62.exec("bowrpg");
          re49.exec("pbagrag");
          re24.exec(str6);
          """/xbadhrebe/""".R.exec(str63);
          """/znp/""".R.exec("jva32");
          """/zbmvyyn/""".R.exec(str63);
          """/zfvr/""".R.exec(str63);
          """/ag\s5\.1/""".R.exec(str63);
          """/bcren/""".R.exec(str63);
          """/fnsnev/""".R.exec(str63);
          """/jva/""".R.exec("jva32");
          """/jvaqbjf/""".R.exec(str63);
      }
      
      
      
      def run() {
        for (i <- 0 until benchIter /*TR was 5*/) {
          //println("block0-"+i)
          runBlock0();
          //println("block1-"+i)
          runBlock1();
          //println("block2-"+i)
          runBlock2();
          //println("block3-"+i)
          runBlock3();
          //println("block4-"+i)
          runBlock4();
          //println("block5-"+i)
          runBlock5();
          //println("block6-"+i)
          runBlock6();
          //println("block7-"+i)
          runBlock7();
          //println("block8-"+i)
          runBlock8();
          //println("block9-"+i)
          runBlock9();
          //println("block10-"+i)
          runBlock10();
          //println("block11-"+i)
          runBlock11();
        }
      }
      
      run()
    }
    
    RegExpBenchmark()
  }
}    
