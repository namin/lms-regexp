package scala.lms.regexp

import org.scalatest._

class TestBitCoded extends FileDiffSuite {
  import BitCoded._
  import Parsing._
  import StagedParsing._

  trait Go extends BitCodedDSL with BitCodedDSLImpl {
    def compileGroups(e: E) = {
      val f = groups(e) _
      val fc = compile(f)
      fc
    }
    def checkCodeGroups(e: E, name: String) = {
      withOutFile(prefix+name) {
        val f = groups(e) _
        codegen.emitSource(f, "Groups", new java.io.PrintWriter(System.out))
      }
      assertFileEqualsCheck(prefix+name)
    }

    def compileMatcher(e: E) = {
      val f = matcher(e) _
      val fc = compile(f)
      fc
    }

    def checkCodeMatcher(e: E, name: String) = {
      withOutFile(prefix+name) {
        val f = matcher(e) _
        codegen.emitSource(f, "Matcher", new java.io.PrintWriter(System.out))
      }
      assertFileEqualsCheck(prefix+name)
    }
  }

  // ((ab)(c|d)|(abc))*
  val exE = Estar(Eplus(
    Eprod(Eprod(Echar('a'), Echar('b')), Eplus(Echar('c'), Echar('d'))),
    Eprod(Echar('a'), Eprod(Echar('b'), Echar('c')))))

  // p1 = [inl ((a; b); inr d); inr (a;(b; c))]
  val ex1 = Vstar(List(
    Vinl(Vprod(Vprod(Vchar('a'), Vchar('b')), Vinr(Echar('c'), Vchar('d'))),
         Eprod(Echar('a'), Eprod(Echar('b'), Echar('c')))),
    Vinr(Eprod(Eprod(Echar('a'), Echar('b')), Eplus(Echar('c'), Echar('d'))),
         Vprod(Vchar('a'), Vprod(Vchar('b'), Vchar('c'))))))(
    Eplus(
      Eprod(Eprod(Echar('a'), Echar('b')), Eplus(Echar('c'), Echar('d'))),
      Eprod(Echar('a'), Eprod(Echar('b'), Echar('c')))))
  // p2 = [inl ((a; b); inr d); inl ((a; b); inl c)] a
  val ex2 = Vstar(List(
    Vinl(Vprod(Vprod(Vchar('a'), Vchar('b')), Vinr(Echar('c'), Vchar('d'))),
         Eprod(Echar('a'), Eprod(Echar('b'), Echar('c')))),
    Vinl(Vprod(Vprod(Vchar('a'), Vchar('b')), Vinl(Vchar('c'), Echar('d'))),
         Eprod(Echar('a'), Eprod(Echar('b'), Echar('c'))))))(
    Eplus(
      Eprod(Eprod(Echar('a'), Echar('b')), Eplus(Echar('c'), Echar('d'))),
      Eprod(Echar('a'), Eprod(Echar('b'), Echar('c')))))

  val ex1code = "001011"
  val ex2code = "0010001"

  val exE2 = Eprod(Echar('a'), Eprod(Estar(Eplus(Echar('b'), Echar('c'))), Echar('a')))

  val exStar1 = Estar(E1)

  val exStar2 = Eprod(Egroup(Estar(Echar('a'))), Estar(Echar('a')))

  def testCode1 = {
    expect(ex1code){str(code(ex1))}
  }

  def testDecode1 = {
    expect(ex1){decode(exE)(unstr(ex1code))}
  }

  def testCode2 = {
    expect(ex2code){str(code(ex2))}
  }

  def testDecode2 = {
    expect(ex2){decode(exE)(unstr(ex2code))}
  }

  def testNfa = {
    val nfa = NFA.fromE(exE)
    val codes = NFA.run(nfa)("abdabc".toList)
    expect(Set(ex1code, ex2code))(codes.map(str))
  }

  def testNfa2 = {
    val nfa = NFA.fromE(exE2)
    val codes = NFA.run(nfa)("abca".toList)
    expect(Set("00011"))(codes.map(str))
  }

  def testEpsilonClosures = {
    val nfa = NFA.fromE(exStar1)
    expect(Map(
      0 -> Map(0 -> List(), 1 -> List(false), 2 -> List(true)),
      1 -> Map(1 -> List(), 0 -> List(), 2 -> List(true)),
      2 -> Map(2 -> List()))){
      DFA.epsilonClosures(nfa)
    }
  }

  def testDFA = {
    val nfa = NFA.fromE(exStar1)
    expect(DFA(1, Set(0), Set(),
      Map(0 -> Set(0, 1, 2)),
      Map(0 -> List(), 1 -> List(false), 2 -> List(true)),
      2)){
      DFA.fromNFA(nfa)
    }
  }

  def testParsing = {
    val nfa = NFA.fromE(exE)
    val dfa = DFA.fromNFA(nfa)
    expect(Some(ex1code)){
      (DFA.run(dfa)("abdabc".toList)).map(str)
    }
    expect(None){
      (DFA.run(dfa)("abdabca".toList)).map(str)
    }
    expect(None){
      (DFA.run(dfa)("abdabcx".toList)).map(str)
    }
  }

  def testParsing2 = {
    val nfa = NFA.fromE(exE2)
    val dfa = DFA.fromNFA(nfa)
    expect(Some("00011")){
      (DFA.run(dfa)("abca".toList)).map(str)
    }
    expect(None){
      (DFA.run(dfa)("abc".toList)).map(str)
    }
  }

  def testParsing3 = {
    val nfa = NFA.fromE(exStar2)
    val dfa = DFA.fromNFA(nfa)
    expect(Some("00011")){
      (DFA.run(dfa)("aaa".toList)).map(str)
    }
  }

  def testGrouping1 = {
    val r = groups(exStar2)(unstr("00011"))
    expect(List(Some(0, 3)).toIndexedSeq){r}
  }

  def testGrouping2 = {
    val e = Estar(Egroup(Estar(Egroup(Egroup(
      Eplus(Egroup(Echar('a')), Eplus(
	    Egroup(Echar('b')),
	    Egroup(Echar('c')))))))))
    val nfa = NFA.fromE(e)
    val dfa = DFA.fromNFA(nfa)
    val c = DFA.run(dfa)("ccc".toList)
    expect(true){c != None}
    val r = groups(e)(c.get)
    expect(List(Some(0, 3), Some(2, 3), Some(2, 3),
      None, None, Some(2, 3)).toIndexedSeq){r}

    val go = new Go {}
    val fc = go.compileGroups(e)
    expect(List((0,3),(2,3),(2,3),null,null,(2,3))){fc(c.get)}

    go.checkCodeGroups(e, "parse_groups_abcstar")

    val fm = go.compileMatcher(e)
    expect(c.get){fm("ccc".toList)}

    go.checkCodeMatcher(e, "parse_matcher_abcstar")
  }
}
