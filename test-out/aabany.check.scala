/*****************************************
  Emitting Generated Code                  
*******************************************/
class Match extends ((Unit)=>(scala.lms.regexp.Automaton[Char, Byte])) {
def apply(x0:Unit): scala.lms.regexp.Automaton[Char, Byte] = {
var x23 = null.asInstanceOf[scala.lms.regexp.Automaton[Char, Byte]]
var x12 = null.asInstanceOf[scala.Function1[Char, scala.lms.regexp.Automaton[Char, Byte]]]
var x26 = null.asInstanceOf[scala.lms.regexp.Automaton[Char, Byte]]
var x15 = null.asInstanceOf[scala.Function1[Char, scala.lms.regexp.Automaton[Char, Byte]]]
var x21 = null.asInstanceOf[scala.lms.regexp.Automaton[Char, Byte]]
var x18 = null.asInstanceOf[scala.Function1[Char, scala.lms.regexp.Automaton[Char, Byte]]]
var x28 = null.asInstanceOf[scala.lms.regexp.Automaton[Char, Byte]]
var x1 = null.asInstanceOf[scala.Function1[Char, scala.lms.regexp.Automaton[Char, Byte]]]
var x32 = null.asInstanceOf[scala.lms.regexp.Automaton[Char, Byte]]
var x4 = null.asInstanceOf[scala.Function1[Char, scala.lms.regexp.Automaton[Char, Byte]]]
var x10 = null.asInstanceOf[scala.lms.regexp.Automaton[Char, Byte]]
var x7 = null.asInstanceOf[scala.Function1[Char, scala.lms.regexp.Automaton[Char, Byte]]]
x18 = {x19: (Char) => 
val x20 = x19 == 'A'
val x24 = if (x20) {
x21
} else {
x23
}
x24
}
x21 = scala.lms.regexp.Automaton(3.toByte,x18)
x15 = {x16: (Char) => 
val x17 = x16 == 'A'
val x25 = if (x17) {
x21
} else {
x23
}
x25
}
x26 = scala.lms.regexp.Automaton(3.toByte,x15)
x12 = {x13: (Char) => 
val x14 = x13 == 'A'
val x27 = if (x14) {
x26
} else {
x23
}
x27
}
x23 = scala.lms.regexp.Automaton(3.toByte,x12)
x7 = {x8: (Char) => 
val x9 = x8 == 'A'
val x30 = if (x9) {
x10
} else {
val x11 = x8 == 'B'
val x29 = if (x11) {
x23
} else {
x28
}
x29
}
x30
}
x10 = scala.lms.regexp.Automaton(0.toByte,x7)
x4 = {x5: (Char) => 
val x6 = x5 == 'A'
val x31 = if (x6) {
x10
} else {
x28
}
x31
}
x32 = scala.lms.regexp.Automaton(0.toByte,x4)
x1 = {x2: (Char) => 
val x3 = x2 == 'A'
val x33 = if (x3) {
x32
} else {
x28
}
x33
}
x28 = scala.lms.regexp.Automaton(0.toByte,x1)
x28
}
}
/*****************************************
  End of Generated Code                  
*******************************************/
compilation: ok
// idx:   0
// out:   0
// char:  X
// idx:   1
// out:   0
// char:  A
// idx:   2
// out:   0
// char:  B
// idx:   3
// out:   0
// char:  Z
// idx:   4
// out:   0
// char:  A
// idx:   5
// out:   0
// char:  A
// idx:   6
// out:   0
// char:  B
// idx:   7
// out:   3
// char:  W
// idx:   8
// out:   3
// char:  A
// idx:   9
// out:   3
// char:  A
// idx:   10
// out:   3
// char:  A
// idx:   11
// out:   3
// char:  A
// idx:   12
// out:   3
// char:  B
// idx:   13
// out:   3
// char:  Q
// idx:   14
// out:   3
