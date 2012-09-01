/*
(((\w+):\/\/)([^\/:]*)(:(\d+))?)?  ([^#?]*)(\?([^#]*))?(#(.*))?
123   3     24       45 6   65 1   7      78  9     98 A B  BA
uggc://jjj.snprobbx.pbz/ybtva.cuc
*/

package scala.virtualization.lms.regexp.backtrack.handopt
class re1matcher1() {
def apply(cp: Int, input: String): Boolean = {
val end = input.length

def isDigit(c: Char): Boolean = '0' <= c && c <= '9';
def isWord(c: Char): Boolean = ('a' <= c && c <= 'z') || ('A' <= c && c <= 'Z') || isDigit(c) || c == '_';

def wordLoop(cp: Int): Boolean = { // ((\w+):\/\/)
  // left par1
  // left par2
  // left par3
  var idx = cp
  while (idx < end && isWord(input.charAt(idx))) {  // todo: backtrack?
    idx += 1
  }
  // right par3
  if (idx + 2 >= end) return false
  if (input.charAt(idx) != ':') return false
  idx += 1
  if (input.charAt(idx) != '/') return false
  idx += 1
  if (input.charAt(idx) != '/') return false
  // right par2
  noSlashNoColonLoop(idx)
}
def noSlashNoColonLoop(cp: Int): Boolean = { // ([^\/:]*)
  // left par4
  var idx = cp
  while (idx < end && { val c = input.charAt(idx); '/' != c && ':' != c})  // todo: backtrack?
    idx += 1
  // right par4
  digitLoop(idx) || rest(idx) // backtrack, par5 is optional
}
def digitLoop(cp: Int): Boolean = { // (:(\d+))?
  // left par5
  var idx = cp
  if (idx + 1 >= end) return false
  if (input.charAt(idx) != ':') return false
  idx += 1
  // left par6
  if (!isDigit(input.charAt(idx))) return false
  idx += 1
  while (idx < end && isDigit(input.charAt(idx))) {  // todo: backtrack?
    idx += 1
  }
  // right par6
  // right par5
  // right par1
  rest(idx)
}
def rest(cp: Int): Boolean = {
  noHashNoQuestLoop(cp)
}
def noHashNoQuestLoop(cp: Int): Boolean = { // ([^#?]*)
  // left par7
  var idx = cp
  while (idx < end && { val c = input.charAt(idx); '#' != c && '?' != c})  // todo: backtrack?
    idx += 1
  // right par7

  //return true //XX
  questNoHashLoop(idx) || success(idx)
}
def questNoHashLoop(cp: Int): Boolean = { // (\?([^#]*))?
  // left par8
  var idx = cp
  if (idx >= end) return false
  if (input.charAt(idx) != '?') return false
  idx += 1
  // left par9
  while (idx < end && { val c = input.charAt(idx); '#' != c})  // todo: backtrack?
    idx += 1
  // right par9
  // right par8
  hashAnyLoop(idx) || success(idx)
}
def hashAnyLoop(cp: Int): Boolean = { // (#(.*))?
  // left parA
  var idx = cp
  if (idx >= end) return false
  if (input.charAt(idx) != '#') return false
  idx += 1
  // left parB
  while (idx < end && { val c = input.charAt(idx); true})  // todo: backtrack?
    idx += 1
  // right parB
  // right parA
  success(idx)
}
def success(cp: Int): Boolean = {
  true
}


wordLoop(cp) || rest(cp) // backtrack, par1 is optional
}
}
