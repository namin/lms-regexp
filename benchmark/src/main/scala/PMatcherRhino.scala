class PMatcherRhino() extends PMatcher {
  import scala.virtualization.lms.regexp.backtrack._
  import Rhino._
  import RhinoMatcher.matcher

  private val fc = new RhinoP1
  private val ex: String = "^((a*ab)|a*(c))$"
  private val re = RhinoParser.compileREStub(ex, "", false)
  re.stmatcher = fc
  private val inp: Int = 0

  def matches(input: String): Boolean = {
    var gData = new REGlobalData

    if (re.parenCount != 0) {
        gData.parens = new Array[Long](re.parenCount);
    } else {
        gData.parens = null;
    }

    gData.backTrackStackTop = null;
    gData.stateStackTop = null;

    gData.multiline = (re.flags & JSREG_MULTILINE) != 0;
    gData.regexp = re;

    var j = 0
    while (j < re.parenCount) {
        gData.parens(j) = -1l;
        j += 1
    }

    matcher.input = input
    matcher.gData = gData
    matcher.re = re

    def findOccurrence(): REGlobalData = {
      val mat = re.stmatcher
      val end = if (re.startNode.op == REOP_BOL) 0 else input.length
      var i = inp
      while (i <= end) {
        gData.skipped = i
        gData.cp = i

        val res = mat()
        if (res) return gData
        i += 1
      }
      null
    }
    findOccurrence() != null
  }
}
