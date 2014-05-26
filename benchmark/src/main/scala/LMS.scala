class MatchAAB extends (String=>Boolean) {
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
else { throw new RuntimeException("invalid state " + id) }
}
}
class MatchAABany extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id = 18
var i = 0
val n_dec = n-1
while (i < n_dec) {
val c = input.charAt(i)
id =
if (id == 18) {
val x19 = c
val x20 = x19 == 'A'
val x50 = if (x20) {
21
} else {
18
}
x50
}
else if (id == 21) {
val x22 = c
val x23 = x22 == 'A'
val x48 = if (x23) {
24
} else {
18
}
x48
}
else if (id == 29) {
val x30 = c
val x31 = x30 == 'A'
val x44 = if (x31) {
return true
} else {
return true
}
x44
}
else if (id == 24) {
val x25 = c
val x26 = x25 == 'A'
val x47 = if (x26) {
24
} else {
val x28 = x25 == 'B'
val x46 = if (x28) {
return true
} else {
18
}
x46
}
x47
}
else if (id == 32) {
val x33 = c
val x34 = x33 == 'A'
val x42 = if (x34) {
return true
} else {
return true
}
x42
}
else if (id == 35) {
val x36 = c
val x37 = x36 == 'A'
val x41 = if (x37) {
return true
} else {
return true
}
x41
}
else { throw new RuntimeException("invalid state " + id) }
i += 1
}
val c = input.charAt(i)
if (id == 18) {
val x19 = c
val x20 = x19 == 'A'
val x50 = if (x20) {
false
} else {
false
}
x50
}
else if (id == 21) {
val x22 = c
val x23 = x22 == 'A'
val x48 = if (x23) {
false
} else {
false
}
x48
}
else if (id == 29) {
val x30 = c
val x31 = x30 == 'A'
val x44 = if (x31) {
true
} else {
true
}
x44
}
else if (id == 24) {
val x25 = c
val x26 = x25 == 'A'
val x47 = if (x26) {
false
} else {
val x28 = x25 == 'B'
val x46 = if (x28) {
true
} else {
false
}
x46
}
x47
}
else if (id == 32) {
val x33 = c
val x34 = x33 == 'A'
val x42 = if (x34) {
true
} else {
true
}
x42
}
else if (id == 35) {
val x36 = c
val x37 = x36 == 'A'
val x41 = if (x37) {
true
} else {
true
}
x41
}
else { throw new RuntimeException("invalid state " + id) }
}
}
class MatchUSD extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id = 51
var i = 0
val n_dec = n-1
while (i < n_dec) {
val c = input.charAt(i)
id =
if (id == 85) {
val x86 = c
return false
}
else if (id == 80) {
val x81 = c
val x82 = '0' <= x81
val x83 = x81 <= '9'
val x84 = x82 && x83
val x89 = if (x84) {
85
} else {
return false
}
x89
}
else if (id == 75) {
val x76 = c
val x77 = '0' <= x76
val x78 = x76 <= '9'
val x79 = x77 && x78
val x91 = if (x79) {
80
} else {
return false
}
x91
}
else if (id == 68) {
val x69 = c
val x70 = '0' <= x69
val x71 = x69 <= '9'
val x72 = x70 && x71
val x94 = if (x72) {
68
} else {
val x74 = x69 == '.'
val x93 = if (x74) {
75
} else {
return false
}
x93
}
x94
}
else if (id == 96) {
val x97 = c
val x98 = '0' <= x97
val x99 = x97 <= '9'
val x100 = x98 && x99
val x101 = if (x100) {
68
} else {
return false
}
x101
}
else if (id == 63) {
val x64 = c
val x65 = '0' <= x64
val x66 = x64 <= '9'
val x67 = x65 && x66
val x106 = if (x67) {
68
} else {
val x95 = x64 == '+'
val x105 = if (x95) {
96
} else {
val x103 = x64 == '-'
val x104 = if (x103) {
96
} else {
return false
}
x104
}
x105
}
x106
}
else if (id == 60) {
val x61 = c
val x62 = x61 == ' '
val x108 = if (x62) {
63
} else {
return false
}
x108
}
else if (id == 57) {
val x58 = c
val x59 = x58 == 'd'
val x110 = if (x59) {
60
} else {
return false
}
x110
}
else if (id == 54) {
val x55 = c
val x56 = x55 == 's'
val x112 = if (x56) {
57
} else {
return false
}
x112
}
else if (id == 51) {
val x52 = c
val x53 = x52 == 'u'
val x114 = if (x53) {
54
} else {
return false
}
x114
}
else { throw new RuntimeException("invalid state " + id) }
i += 1
}
val c = input.charAt(i)
if (id == 85) {
val x86 = c
false
}
else if (id == 80) {
val x81 = c
val x82 = '0' <= x81
val x83 = x81 <= '9'
val x84 = x82 && x83
val x89 = if (x84) {
true
} else {
false
}
x89
}
else if (id == 75) {
val x76 = c
val x77 = '0' <= x76
val x78 = x76 <= '9'
val x79 = x77 && x78
val x91 = if (x79) {
false
} else {
false
}
x91
}
else if (id == 68) {
val x69 = c
val x70 = '0' <= x69
val x71 = x69 <= '9'
val x72 = x70 && x71
val x94 = if (x72) {
false
} else {
val x74 = x69 == '.'
val x93 = if (x74) {
false
} else {
false
}
x93
}
x94
}
else if (id == 96) {
val x97 = c
val x98 = '0' <= x97
val x99 = x97 <= '9'
val x100 = x98 && x99
val x101 = if (x100) {
false
} else {
false
}
x101
}
else if (id == 63) {
val x64 = c
val x65 = '0' <= x64
val x66 = x64 <= '9'
val x67 = x65 && x66
val x106 = if (x67) {
false
} else {
val x95 = x64 == '+'
val x105 = if (x95) {
false
} else {
val x103 = x64 == '-'
val x104 = if (x103) {
false
} else {
false
}
x104
}
x105
}
x106
}
else if (id == 60) {
val x61 = c
val x62 = x61 == ' '
val x108 = if (x62) {
false
} else {
false
}
x108
}
else if (id == 57) {
val x58 = c
val x59 = x58 == 'd'
val x110 = if (x59) {
false
} else {
false
}
x110
}
else if (id == 54) {
val x55 = c
val x56 = x55 == 's'
val x112 = if (x56) {
false
} else {
false
}
x112
}
else if (id == 51) {
val x52 = c
val x53 = x52 == 'u'
val x114 = if (x53) {
false
} else {
false
}
x114
}
else { throw new RuntimeException("invalid state " + id) }
}
}
class MatchAnything extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return true
var id = return true
var i = 0
val n_dec = n-1
while (i < n_dec) {
val c = input.charAt(i)
id =
if (id == 116) {
val x117 = c
return true
}
else { throw new RuntimeException("invalid state " + id) }
i += 1
}
val c = input.charAt(i)
if (id == 116) {
val x117 = c
true
}
else { throw new RuntimeException("invalid state " + id) }
}
}
class MatchCook extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id = 120
var i = 0
val n_dec = n-1
while (i < n_dec) {
val c = input.charAt(i)
id =
if (id == 85) {
val x86 = c
return false
}
else if (id == 120) {
val x121 = c
val x122 = x121 == 'A'
val x140 = if (x122) {
123
} else {
val x138 = x121 == 'B'
val x139 = if (x138) {
120
} else {
return false
}
x139
}
x140
}
else if (id == 123) {
val x124 = c
val x125 = x124 == 'A'
val x137 = if (x125) {
123
} else {
val x127 = x124 == 'B'
val x136 = if (x127) {
128
} else {
return false
}
x136
}
x137
}
else if (id == 128) {
val x129 = c
val x130 = x129 == 'A'
val x134 = if (x130) {
123
} else {
val x131 = x129 == 'B'
val x133 = if (x131) {
120
} else {
return false
}
x133
}
x134
}
else { throw new RuntimeException("invalid state " + id) }
i += 1
}
val c = input.charAt(i)
if (id == 85) {
val x86 = c
false
}
else if (id == 120) {
val x121 = c
val x122 = x121 == 'A'
val x140 = if (x122) {
true
} else {
val x138 = x121 == 'B'
val x139 = if (x138) {
true
} else {
false
}
x139
}
x140
}
else if (id == 123) {
val x124 = c
val x125 = x124 == 'A'
val x137 = if (x125) {
true
} else {
val x127 = x124 == 'B'
val x136 = if (x127) {
true
} else {
false
}
x136
}
x137
}
else if (id == 128) {
val x129 = c
val x130 = x129 == 'A'
val x134 = if (x130) {
true
} else {
val x131 = x129 == 'B'
val x133 = if (x131) {
true
} else {
false
}
x133
}
x134
}
else { throw new RuntimeException("invalid state " + id) }
}
}
