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
val x180 = if (x5) {
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
def x27(): Boolean = {
val x29 = x1.cp
val x30 = x1.cp
val x32 = x30 == x17
val x41 = if (x32) {
false
} else {
val x33 = x1.cp
val x34 = x16.charAt(x33)
val x35 = 'b' == x34
val x36 = if (x35) {
true
} else {
x35
}
val x39 = if (x36) {
val x37 = x1.cp += 1
true
} else {
false
}
x39
}
val x55 = if (x41) {
val x42 = x1.parensIndex(1)
val x43 = x1.parensLength(1)
val x44 = x1.cp
val x45 = x44 - x23
val x46 = x1.setParens(1,x23,x45)
val x47 = x7()
val x50 = if (x47) {
true
} else {
val x48 = x1.setParens(1,x42,x43)
false
}
val x53 = if (x50) {
true
} else {
val x51 = x1.cp = x29
false
}
x53
} else {
false
}
x55
}
def x57(): Boolean = {
val x59 = x1.cp
val x60 = x1.cp
val x62 = x60 == x17
val x71 = if (x62) {
false
} else {
val x63 = x1.cp
val x64 = x16.charAt(x63)
val x65 = 'a' == x64
val x66 = if (x65) {
true
} else {
x65
}
val x69 = if (x66) {
val x67 = x1.cp += 1
true
} else {
false
}
x69
}
val x77 = if (x71) {
val x72 = x57()
val x75 = if (x72) {
true
} else {
val x73 = x1.cp = x59
false
}
x75
} else {
false
}
val x80 = if (x77) {
true
} else {
val x78 = x27()
x78
}
x80
}
def x84(): Boolean = {
val x86 = x1.cp
val x87 = x1.cp
val x89 = x87 == x17
val x98 = if (x89) {
false
} else {
val x90 = x1.cp
val x91 = x16.charAt(x90)
val x92 = 'a' == x91
val x93 = if (x92) {
true
} else {
x92
}
val x96 = if (x93) {
val x94 = x1.cp += 1
true
} else {
false
}
x96
}
val x104 = if (x98) {
val x99 = x84()
val x102 = if (x99) {
true
} else {
val x100 = x1.cp = x86
false
}
x102
} else {
false
}
val x107 = if (x104) {
true
} else {
val x105 = x57()
x105
}
x107
}
val x24 = x1.cp
val x109 = x84()
val x112 = if (x109) {
true
} else {
val x110 = x1.cp = x24
false
}
val x175 = if (x112) {
true
} else {
def x114(): Boolean = {
val x116 = x1.cp
val x117 = x1.cp
val x118 = x1.cp
val x120 = x118 == x17
val x129 = if (x120) {
false
} else {
val x121 = x1.cp
val x122 = x16.charAt(x121)
val x123 = 'c' == x122
val x124 = if (x123) {
true
} else {
x123
}
val x127 = if (x124) {
val x125 = x1.cp += 1
true
} else {
false
}
x127
}
val x143 = if (x129) {
val x130 = x1.parensIndex(2)
val x131 = x1.parensLength(2)
val x132 = x1.cp
val x133 = x132 - x116
val x134 = x1.setParens(2,x116,x133)
val x135 = x7()
val x138 = if (x135) {
true
} else {
val x136 = x1.setParens(2,x130,x131)
false
}
val x141 = if (x138) {
true
} else {
val x139 = x1.cp = x117
false
}
x141
} else {
false
}
x143
}
def x145(): Boolean = {
val x147 = x1.cp
val x148 = x1.cp
val x150 = x148 == x17
val x159 = if (x150) {
false
} else {
val x151 = x1.cp
val x152 = x16.charAt(x151)
val x153 = 'a' == x152
val x154 = if (x153) {
true
} else {
x153
}
val x157 = if (x154) {
val x155 = x1.cp += 1
true
} else {
false
}
x157
}
val x165 = if (x159) {
val x160 = x145()
val x163 = if (x160) {
true
} else {
val x161 = x1.cp = x147
false
}
x163
} else {
false
}
val x168 = if (x165) {
true
} else {
val x166 = x114()
x166
}
x168
}
val x113 = x1.cp
val x170 = x145()
val x173 = if (x170) {
true
} else {
val x171 = x1.cp = x113
false
}
x173
}
val x178 = if (x175) {
true
} else {
val x176 = x1.cp = x3
false
}
x178
} else {
false
}
val x183 = if (x180) {
true
} else {
val x181 = x1.cp = x2
false
}
x183
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
