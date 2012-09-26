package scala.virtualization.lms.regexp

import org.scalatest._

class TestBitCoded extends Suite {
  import BitCoded._
  import Parsing._

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
}
