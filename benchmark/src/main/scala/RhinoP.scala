/*****************************************
  Emitting Generated Code                  
*******************************************/
import scala.virtualization.lms.regexp.backtrack._
import RhinoMatcher.matcher
class RhinoP1 extends ((Unit)=>(Boolean)) {
def apply(x0:Unit): Boolean = {
val x1 = matcher.gData
val x2 = x1.cp
val x3 = x1.cp
val x4 = x1.cp
val x5 = x4 == 0
val x140 = if (x5) {
val x16 = matcher.input
val x17 = x16.length
val x6 = x1.cp
val x23 = x1.cp
def x7(): Boolean = {
val x9 = x1.parensIndex(0)
val x10 = x1.parensLength(0)
val x11 = x1.cp
val x12 = x11 - x6
val x13 = x1.setParens(0,x6,x12)
val x14 = x1.cp
val x15 = x1.cp
val x18 = x15 == x17
val x21 = if (x18) {
true
} else {
val x19 = x1.setParens(0,x9,x10)
false
}
x21
}
def x25(): Boolean = {
val x27 = x1.cp
val x28 = RhinoMatchUtil.flatNMatcher(x1,5,2,x16,x17) // ab
val x42 = if (x28) {
val x29 = x1.parensIndex(1)
val x30 = x1.parensLength(1)
val x31 = x1.cp
val x32 = x31 - x23
val x33 = x1.setParens(1,x23,x32)
val x34 = x7()
val x37 = if (x34) {
true
} else {
val x35 = x1.setParens(1,x29,x30)
false
}
val x40 = if (x37) {
true
} else {
val x38 = x1.cp = x27
false
}
x40
} else {
false
}
x42
}
def x44(): Boolean = {
val x46 = x1.cp
val x47 = x1.cp
val x49 = x47 == x17
val x58 = if (x49) {
false
} else {
val x50 = x1.cp
val x51 = x16.charAt(x50)
val x52 = 'a' == x51
val x53 = if (x52) {
true
} else {
x52
}
val x56 = if (x53) {
val x54 = x1.cp += 1
true
} else {
false
}
x56
}
val x64 = if (x58) {
val x59 = x44()
val x62 = if (x59) {
true
} else {
val x60 = x1.cp = x46
false
}
x62
} else {
false
}
val x67 = if (x64) {
true
} else {
val x65 = x25()
x65
}
x67
}
val x24 = x1.cp
val x69 = x44()
val x72 = if (x69) {
true
} else {
val x70 = x1.cp = x24
false
}
val x135 = if (x72) {
true
} else {
def x74(): Boolean = {
val x76 = x1.cp
val x77 = x1.cp
val x78 = x1.cp
val x80 = x78 == x17
val x89 = if (x80) {
false
} else {
val x81 = x1.cp
val x82 = x16.charAt(x81)
val x83 = 'c' == x82
val x84 = if (x83) {
true
} else {
x83
}
val x87 = if (x84) {
val x85 = x1.cp += 1
true
} else {
false
}
x87
}
val x103 = if (x89) {
val x90 = x1.parensIndex(2)
val x91 = x1.parensLength(2)
val x92 = x1.cp
val x93 = x92 - x76
val x94 = x1.setParens(2,x76,x93)
val x95 = x7()
val x98 = if (x95) {
true
} else {
val x96 = x1.setParens(2,x90,x91)
false
}
val x101 = if (x98) {
true
} else {
val x99 = x1.cp = x77
false
}
x101
} else {
false
}
x103
}
def x105(): Boolean = {
val x107 = x1.cp
val x108 = x1.cp
val x110 = x108 == x17
val x119 = if (x110) {
false
} else {
val x111 = x1.cp
val x112 = x16.charAt(x111)
val x113 = 'a' == x112
val x114 = if (x113) {
true
} else {
x113
}
val x117 = if (x114) {
val x115 = x1.cp += 1
true
} else {
false
}
x117
}
val x125 = if (x119) {
val x120 = x105()
val x123 = if (x120) {
true
} else {
val x121 = x1.cp = x107
false
}
x123
} else {
false
}
val x128 = if (x125) {
true
} else {
val x126 = x74()
x126
}
x128
}
val x73 = x1.cp
val x130 = x105()
val x133 = if (x130) {
true
} else {
val x131 = x1.cp = x73
false
}
x133
}
val x138 = if (x135) {
true
} else {
val x136 = x1.cp = x3
false
}
x138
} else {
false
}
val x143 = if (x140) {
true
} else {
val x141 = x1.cp = x2
false
}
x143
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
