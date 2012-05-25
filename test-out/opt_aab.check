class Match extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id = 0
var i = 0
val n_dec = n-1
while (i < n_dec) {
val c = input.charAt(i)
id =
if (id == 0) {
val x1 = c
val x2 = x1 == 'A'
val x17 = if (x2) {
3
} else {
0
}
x17
}
else if (id == 6) {
val x7 = c
val x8 = x7 == 'A'
val x14 = if (x8) {
6
} else {
val x10 = x7 == 'B'
val x13 = if (x10) {
0
} else {
0
}
x13
}
x14
}
else if (id == 3) {
val x4 = c
val x5 = x4 == 'A'
val x15 = if (x5) {
6
} else {
0
}
x15
}
else { throw new RuntimeException("invalid state " + id) }
i += 1
}
val c = input.charAt(i)
if (id == 0) {
val x1 = c
val x2 = x1 == 'A'
val x17 = if (x2) {
false
} else {
false
}
x17
}
else if (id == 6) {
val x7 = c
val x8 = x7 == 'A'
val x14 = if (x8) {
false
} else {
val x10 = x7 == 'B'
val x13 = if (x10) {
true
} else {
false
}
x13
}
x14
}
else if (id == 3) {
val x4 = c
val x5 = x4 == 'A'
val x15 = if (x5) {
false
} else {
false
}
x15
}
else { throw new RuntimeException("invalid state " + id) }
}
}
