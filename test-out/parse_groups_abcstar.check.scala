/*****************************************
  Emitting Generated Code                  
*******************************************/
class Groups extends ((scala.collection.immutable.List[Boolean])=>(scala.collection.immutable.List[Tuple2[Int,Int]])) {
def apply(x84:scala.collection.immutable.List[Boolean]): scala.collection.immutable.List[Tuple2[Int,Int]] = {
var x85: scala.collection.immutable.List[Tuple2[Int,Int]] = List(null, null, null, null, null, null)
var x86: Int = 0
var x87: scala.collection.immutable.List[Boolean] = x84
val x27 = List()
val x160 = while ({val x88 = x87
val x89 = x88.head
val x90 = !x89
x90}) {
val x92 = x87
val x94 = x86
var x96: scala.collection.immutable.List[Tuple2[Int,Int]] = List(null, null, null, null, null)
var x97: Int = 0
val x93 = x92.tail
var x98: scala.collection.immutable.List[Boolean] = x93
val x95 = 0 + x94
val x145 = while ({val x99 = x98
val x100 = x99.head
val x101 = !x100
x101}) {
val x103 = x98
val x105 = x97
val x106 = x95 + x105
val x104 = x103.tail
val x107 = x104.head
val x130 = if (x107) {
val x108 = x104.tail
val x109 = x108.head
val x120 = if (x109) {
val x110 = x108.tail
val x112 = x106 + 1
val x113 = (x106,x112)
val x114 = x113 :: x27
val x116 = List(null) ::: x114
val x117 = (x116,1,x110)
x117
} else {
val x110 = x108.tail
val x112 = x106 + 1
val x113 = (x106,x112)
val x114 = x113 :: x27
val x118 = x114 ::: List(null)
val x119 = (x118,1,x110)
x119
}
val x122 = x120._2
val x123 = x120._3
val x121 = x120._1
val x124 = List(null) ::: x121
val x125 = (x124,x122,x123)
x125
} else {
val x108 = x104.tail
val x112 = x106 + 1
val x113 = (x106,x112)
val x114 = x113 :: x27
val x128 = x114 ::: List(null, null)
val x129 = (x128,1,x108)
x129
}
val x132 = x130._2
val x134 = x106 + x132
val x135 = (x106,x134)
val x131 = x130._1
val x136 = x135 :: x131
val x138 = x135 :: x136
x96 = x138
val x133 = x130._3
x98 = x133
val x142 = x105 + x132
x97 = x142
()
}
val x146 = x96
val x147 = x97
val x148 = x98
val x151 = x95 + x147
val x152 = (x95,x151)
val x153 = x152 :: x146
x85 = x153
val x149 = x148.tail
x87 = x149
val x157 = x94 + x147
x86 = x157
()
}
val x161 = x85
val x162 = x86
val x163 = x87
x161
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
