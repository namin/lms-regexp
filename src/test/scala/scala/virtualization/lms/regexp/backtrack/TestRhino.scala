package scala.virtualization.lms.regexp.backtrack

import scala.virtualization.lms.regexp.FileDiffSuite



class TestRhino extends FileDiffSuite {

  def run(re: String, in: String) = {
    RhinoMatcher.stmatcher.IR.dumpGeneratedCode = true
    
    println(re + " @ " + in)

    val rno = RhinoParser.compileREStub(re, "", false)

    val res1 = RhinoMatcher.matchNaive(rno, in, 0)
    val str1 = if (res1 == null) "null" else res1 + "/" + (res1.groups(in).mkString(","))
    println(str1)

    val res2 = RhinoMatcher.matchStaged(rno, in, 0)
    val str2 = if (res2 == null) "null" else res2 + "/" + (res2.groups(in).mkString(","))
    println(str2)
    
    expect(str1)(str2)

    println("done")
  }

  def test1 = withOutFile("test-out/rhino1") {
    run("""aab|aac""", "aaac")
  }

  def test2 = withOutFile("test-out/rhino2") {
    run("""\w*b""", "aaabc")
  }

  def test3 = withOutFile("test-out/rhino3") {
    run("""(\w{4})b""", "aaaaaaabc")
  }

  def test4 = withOutFile("test-out/rhino4") {
    // 3000 - 4000 ms
    // (why?)
    run("""(((\w+):\/\/)([^\/:]*)(:(\d+))?)?([^#?]*)(\?([^#]*))?(#(.*))?""",
    "uggc://jjj.snprobbx.pbz/ybtva.cuc")
  }

  def test5 = withOutFile("test-out/rhino5") {
    // 800 ms
    // need to locate Nccyr
    run("""(?:ZFVR.(\d+\.\d+))|(?:(?:Sversbk|TenaCnenqvfb|Vprjrnfry).(\d+\.\d+))|(?:Bcren.(\d+\.\d+))|(?:NccyrJroXvg.(\d+(?:\.\d+)?))""",
    "Zbmvyyn/5.0 (Jvaqbjf; H; Jvaqbjf AG 5.1; ra-HF) NccyrJroXvg/528.9 (XUGZY, yvxr Trpxb) Puebzr/2.0.157.0 Fnsnev/528.9")
  }

  def test6 = withOutFile("test-out/rhino6") {
    run("""qqqq|qqq|qq|q|ZZZZ|ZZZ|ZZ|Z|llll|ll|l|uu|u|UU|U|zz|z|ff|f|gg|g|sss|ss|s|mmm|mm|m""",
    "qqqq, ZZZ q, llll")
  }

  def test7 = withOutFile("test-out/rhino7") {
    run("""(\\\"|\x00-|\x1f|\x7f-|\x9f|\u00ad|\u0600-|\u0604|\u070f|\u17b4|\u17b5|\u200c-|\u200f|\u2028-|\u202f|\u2060-|\u206f|\ufeff|\ufff0-|\uffff)""",
    "GnoThvq")
  }

  def test8 = withOutFile("test-out/rhino8") {
    // this one was failing when switching to scala M7 / lms 0.3
    run("""([a-zA-Z]|\s)+""", "Fubpxjnir Synfu 9.0  e115")
  }
  
}
