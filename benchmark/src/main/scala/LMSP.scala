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
val x68 = while ({val x6 = x1
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
val x37 = if (x19) {
val x16 = x15.head
val x20 = x16 == 'a'
val x27 = if (x20) {
x2 = 1
x5 = false
val x23 = x4
val x24 = Map(5 -> (4,List()), 10 -> (8,List(true)), 1 -> (2,List()), 9 -> (8,List()), 2 -> (2,List(false)), 7 -> (8,List()), 3 -> (2,List()), 8 -> (8,List(false)), 4 -> (2,List(true))) :: x23
x4 = x24
()
} else {
()
}
val x28 = x16 == 'c'
val x35 = if (x28) {
x2 = 2
x5 = true
val x31 = x4
val x32 = Map(11 -> (10,List()), 12 -> (10,List())) :: x31
x4 = x32
()
} else {
()
}
()
} else {
()
}
val x38 = x13 == 1
val x62 = if (x38) {
val x16 = x15.head
val x20 = x16 == 'a'
val x45 = if (x20) {
x2 = 1
x5 = false
val x41 = x4
val x42 = Map(5 -> (4,List()), 10 -> (8,List(true)), 1 -> (2,List()), 9 -> (8,List()), 2 -> (2,List(false)), 7 -> (8,List()), 3 -> (2,List()), 8 -> (8,List(false)), 4 -> (2,List(true))) :: x41
x4 = x42
()
} else {
()
}
val x46 = x16 == 'b'
val x53 = if (x46) {
x2 = 3
x5 = true
val x49 = x4
val x50 = Map(6 -> (5,List()), 12 -> (5,List())) :: x49
x4 = x50
()
} else {
()
}
val x28 = x16 == 'c'
val x60 = if (x28) {
x2 = 2
x5 = true
val x56 = x4
val x57 = Map(11 -> (10,List()), 12 -> (10,List())) :: x56
x4 = x57
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
val x69 = x2
val x71 = x5
val x70 = x69 == -1
val x72 = !x71
val x73 = x70 || x72
val x139 = if (x73) {
null
} else {
var x74: scala.collection.immutable.List[Boolean] = x3
var x75: Int = 12
val x93 = while ({val x76 = x4
val x77 = x76.isEmpty
val x78 = !x77
x78}) {
val x80 = x4
val x82 = x80.tail
x4 = x82
val x84 = x75
val x81 = x80.head
val x85 = x81(x84)
val x88 = x74
val x87 = x85._2
val x89 = x87 ::: x88
x74 = x89
val x86 = x85._1
x75 = x86
()
}
val x94 = x75
val x95 = x94 == 0
val x100 = if (x95) {
val x96 = x74
val x97 = List() ::: x96
x74 = x97
()
} else {
()
}
val x101 = x94 == 10
val x106 = if (x101) {
val x102 = x74
val x103 = List(true, true) ::: x102
x74 = x103
()
} else {
()
}
val x107 = x94 == 1
val x112 = if (x107) {
val x108 = x74
val x109 = List(false) ::: x108
x74 = x109
()
} else {
()
}
val x113 = x94 == 2
val x118 = if (x113) {
val x114 = x74
val x115 = List(false, false) ::: x114
x74 = x115
()
} else {
()
}
val x119 = x94 == 7
val x124 = if (x119) {
val x120 = x74
val x121 = List(true) ::: x120
x74 = x121
()
} else {
()
}
val x125 = x94 == 8
val x130 = if (x125) {
val x126 = x74
val x127 = List(true, false) ::: x126
x74 = x127
()
} else {
()
}
val x131 = x94 == 4
val x136 = if (x131) {
val x132 = x74
val x133 = List(false, true) ::: x132
x74 = x133
()
} else {
()
}
val x137 = x74
x137
}
val x140 = x139 == null
val x220 = if (x140) {
x139
} else {
val x141 = x139.head
val x216 = if (x141) {
var x143: scala.collection.immutable.List[scala.Tuple2[Int, Int]] = List()
var x144: Int = 0
val x142 = x139.tail
var x145: scala.collection.immutable.List[Boolean] = x142
val x160 = while ({val x146 = x145
val x147 = x146.head
val x148 = !x147
x148}) {
val x150 = x145
val x152 = x144
x143 = x3
val x151 = x150.tail
x145 = x151
val x157 = x152 + 1
x144 = x157
()
}
val x161 = x143
val x162 = x144
val x163 = x145
val x164 = x163.tail
val x173 = x162 + 1
val x166 = 0 + x162
val x168 = x166 + 1
val x169 = (x166,x168)
val x170 = x169 :: x3
val x172 = x161 ::: x170
val x175 = List(null) ::: x172
val x176 = (x175,x173,x164)
x176
} else {
var x178: scala.collection.immutable.List[scala.Tuple2[Int, Int]] = List()
var x179: Int = 0
val x142 = x139.tail
var x180: scala.collection.immutable.List[Boolean] = x142
val x195 = while ({val x181 = x180
val x182 = x181.head
val x183 = !x182
x183}) {
val x185 = x180
val x187 = x179
x178 = x3
val x186 = x185.tail
x180 = x186
val x192 = x187 + 1
x179 = x192
()
}
val x196 = x178
val x197 = x179
val x198 = x180
val x199 = x198.tail
val x208 = 1 + 1
val x211 = x197 + x208
val x201 = 0 + x197
val x202 = (0,x201)
val x203 = x202 :: x196
val x207 = x3 ::: x3
val x210 = x203 ::: x207
val x213 = x210 ::: List(null)
val x214 = (x213,x211,x199)
x214
}
val x217 = x216._1
x217
}
x220
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
