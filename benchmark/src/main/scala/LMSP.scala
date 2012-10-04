/*****************************************
  Emitting Generated Code                  
*******************************************/
class PMatch1 extends ((scala.collection.immutable.List[Char])=>(scala.collection.immutable.List[Any])) {
def apply(x0:scala.collection.immutable.List[Char]): scala.collection.immutable.List[Any] = {
var x1: scala.collection.immutable.List[Char] = x0
var x2: Int = 0
val x3 = List()
var x4: scala.collection.immutable.List[scala.Function1[Int, scala.Tuple2[Int, scala.collection.immutable.List[Boolean]]]] = x3
var x5: Boolean = false
val x75 = while ({val x6 = x1
val x9 = x2
val x7 = x6.isEmpty
val x8 = !x7
val x10 = x9 != -1
val x11 = x8 && x10
x11}) {
val x13 = x2
x2 = -1
val x15 = x1
val x17 = x15.tail
x1 = x17
val x19 = x13 == 0
val x45 = if (x19) {
val x16 = x15.head
val x20 = x16 == 'b'
val x27 = if (x20) {
x2 = 1
x5 = true
val x23 = x4
val x24 = Map(8 -> (7,List()), 14 -> (7,List())) :: x23
x4 = x24
()
} else {
()
}
val x28 = x16 == 'c'
val x35 = if (x28) {
x2 = 3
x5 = true
val x31 = x4
val x32 = Map(14 -> (12,List()), 13 -> (12,List())) :: x31
x4 = x32
()
} else {
()
}
val x36 = x16 == 'a'
val x43 = if (x36) {
x2 = 2
x5 = false
val x39 = x4
val x40 = Map(5 -> (5,List(false)), 10 -> (10,List(false)), 1 -> (2,List()), 6 -> (5,List()), 9 -> (10,List()), 2 -> (2,List(false)), 12 -> (10,List(true)), 7 -> (5,List(true)), 3 -> (2,List()), 11 -> (10,List()), 4 -> (5,List())) :: x39
x4 = x40
()
} else {
()
}
()
} else {
()
}
val x48 = x13 == 2
val x71 = if (x48) {
val x16 = x15.head
val x36 = x16 == 'a'
val x55 = if (x36) {
x2 = 2
x5 = false
val x51 = x4
val x52 = Map(5 -> (5,List(false)), 10 -> (10,List(false)), 1 -> (2,List()), 6 -> (5,List()), 9 -> (10,List()), 2 -> (2,List(false)), 12 -> (10,List(true)), 7 -> (5,List(true)), 3 -> (2,List()), 11 -> (10,List()), 4 -> (5,List())) :: x51
x4 = x52
()
} else {
()
}
val x28 = x16 == 'c'
val x62 = if (x28) {
x2 = 3
x5 = true
val x58 = x4
val x59 = Map(14 -> (12,List()), 13 -> (12,List())) :: x58
x4 = x59
()
} else {
()
}
val x20 = x16 == 'b'
val x69 = if (x20) {
x2 = 1
x5 = true
val x65 = x4
val x66 = Map(8 -> (7,List()), 14 -> (7,List())) :: x65
x4 = x66
()
} else {
()
}
()
} else {
()
}
()
}
val x76 = x2
val x78 = x5
val x77 = x76 == -1
val x79 = !x78
val x80 = x77 || x79
val x158 = if (x80) {
null
} else {
var x81: scala.collection.immutable.List[Boolean] = x3
var x82: Int = 14
val x100 = while ({val x83 = x4
val x84 = x83.isEmpty
val x85 = !x84
x85}) {
val x87 = x4
val x89 = x87.tail
x4 = x89
val x91 = x82
val x88 = x87.head
val x92 = x88(x91)
val x95 = x81
val x94 = x92._2
val x96 = x94 ::: x95
x81 = x96
val x93 = x92._1
x82 = x93
()
}
val x101 = x82
val x102 = x101 == 0
val x107 = if (x102) {
val x103 = x81
val x104 = List() ::: x103
x81 = x104
()
} else {
()
}
val x108 = x101 == 5
val x113 = if (x108) {
val x109 = x81
val x110 = List(false, true, false) ::: x109
x81 = x110
()
} else {
()
}
val x114 = x101 == 10
val x119 = if (x114) {
val x115 = x81
val x116 = List(true, false) ::: x115
x81 = x116
()
} else {
()
}
val x120 = x101 == 1
val x125 = if (x120) {
val x121 = x81
val x122 = List(false) ::: x121
x81 = x122
()
} else {
()
}
val x126 = x101 == 9
val x131 = if (x126) {
val x127 = x81
val x128 = List(true) ::: x127
x81 = x128
()
} else {
()
}
val x132 = x101 == 2
val x137 = if (x132) {
val x133 = x81
val x134 = List(false, false) ::: x133
x81 = x134
()
} else {
()
}
val x138 = x101 == 12
val x143 = if (x138) {
val x139 = x81
val x140 = List(true, true) ::: x139
x81 = x140
()
} else {
()
}
val x144 = x101 == 7
val x149 = if (x144) {
val x145 = x81
val x146 = List(false, true, true) ::: x145
x81 = x146
()
} else {
()
}
val x150 = x101 == 4
val x155 = if (x150) {
val x151 = x81
val x152 = List(false, true) ::: x151
x81 = x152
()
} else {
()
}
val x156 = x81
x156
}
val x159 = x158 == null
val x262 = if (x159) {
x158
} else {
val x160 = x158.head
val x258 = if (x160) {
var x162: scala.collection.immutable.List[scala.Tuple2[Int, Int]] = List()
var x163: Int = 0
val x161 = x158.tail
var x164: scala.collection.immutable.List[Boolean] = x161
val x179 = while ({val x165 = x164
val x166 = x165.head
val x167 = !x166
x167}) {
val x169 = x164
val x171 = x163
x162 = x3
val x170 = x169.tail
x164 = x170
val x176 = x171 + 1
x163 = x176
()
}
val x180 = x162
val x181 = x163
val x182 = x164
val x183 = x182.tail
val x192 = x181 + 1
val x185 = 0 + x181
val x187 = x185 + 1
val x188 = (x185,x187)
val x189 = x188 :: x3
val x191 = x180 ::: x189
val x194 = List(null) ::: x191
val x195 = (x194,x192,x183)
x195
} else {
var x197: scala.collection.immutable.List[scala.Tuple2[Int, Int]] = List()
var x198: Int = 0
val x161 = x158.tail
var x199: scala.collection.immutable.List[Boolean] = x161
val x214 = while ({val x200 = x199
val x201 = x200.head
val x202 = !x201
x202}) {
val x204 = x199
val x206 = x198
x197 = x3
val x205 = x204.tail
x199 = x205
val x211 = x206 + 1
x198 = x211
()
}
val x215 = x197
val x216 = x198
val x217 = x199
var x224: scala.collection.immutable.List[scala.Tuple2[Int, Int]] = List()
var x225: Int = 0
val x218 = x217.tail
var x226: scala.collection.immutable.List[Boolean] = x218
val x241 = while ({val x227 = x226
val x228 = x227.head
val x229 = !x228
x229}) {
val x231 = x226
val x233 = x225
x224 = x3
val x232 = x231.tail
x226 = x232
val x238 = x233 + 1
x225 = x238
()
}
val x242 = x224
val x243 = x225
val x244 = x226
val x245 = x244.tail
val x250 = x243 + 1
val x253 = x216 + x250
val x220 = 0 + x216
val x221 = (0,x220)
val x222 = x221 :: x215
val x249 = x242 ::: x3
val x252 = x222 ::: x249
val x255 = x252 ::: List(null)
val x256 = (x255,x253,x245)
x256
}
val x259 = x258._1
x259
}
x262
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
