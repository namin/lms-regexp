class MatchAAB extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id = 0
var i = 0
val n_dec = n-1
while (i < n_dec) {
val x18 = input.charAt(i)
id =
if (id == 0) {
val x26 = x18 == 'A'
val x27 = if (x26) {
3
} else {
0
}
x27
}
else if (id == 6) {
val x26 = x18 == 'A'
val x30 = if (x26) {
6
} else {
val x28 = x18 == 'B'
val x29 = if (x28) {
0
} else {
0
}
x29
}
x30
}
else if (id == 3) {
val x26 = x18 == 'A'
val x31 = if (x26) {
6
} else {
0
}
x31
}
else { throw new RuntimeException("invalid state " + id) }
i += 1
}
val x18 = input.charAt(i)
if (id == 0) {
val x26 = x18 == 'A'
val x27 = if (x26) {
false
} else {
false
}
x27
}
else if (id == 6) {
val x26 = x18 == 'A'
val x30 = if (x26) {
false
} else {
val x28 = x18 == 'B'
val x29 = if (x28) {
true
} else {
false
}
x29
}
x30
}
else if (id == 3) {
val x26 = x18 == 'A'
val x31 = if (x26) {
false
} else {
false
}
x31
}
else { throw new RuntimeException("invalid state " + id) }
}
}
class MatchAABany extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id = 32
var i = 0
val n_dec = n-1
while (i < n_dec) {
val x65 = input.charAt(i)
id =
if (id == 49) {
val x78 = x65 == 'A'
val x79 = if (x78) {
return true
} else {
return true
}
x79
}
else if (id == 46) {
val x78 = x65 == 'A'
val x79 = if (x78) {
return true
} else {
return true
}
x79
}
else if (id == 43) {
val x78 = x65 == 'A'
val x80 = if (x78) {
return true
} else {
return true
}
x80
}
else if (id == 38) {
val x78 = x65 == 'A'
val x83 = if (x78) {
38
} else {
val x81 = x65 == 'B'
val x82 = if (x81) {
return true
} else {
32
}
x82
}
x83
}
else if (id == 35) {
val x78 = x65 == 'A'
val x84 = if (x78) {
38
} else {
32
}
x84
}
else if (id == 32) {
val x78 = x65 == 'A'
val x85 = if (x78) {
35
} else {
32
}
x85
}
else { throw new RuntimeException("invalid state " + id) }
i += 1
}
val x65 = input.charAt(i)
if (id == 49) {
val x78 = x65 == 'A'
val x79 = if (x78) {
true
} else {
true
}
x79
}
else if (id == 46) {
val x78 = x65 == 'A'
val x79 = if (x78) {
true
} else {
true
}
x79
}
else if (id == 43) {
val x78 = x65 == 'A'
val x80 = if (x78) {
true
} else {
true
}
x80
}
else if (id == 38) {
val x78 = x65 == 'A'
val x83 = if (x78) {
false
} else {
val x81 = x65 == 'B'
val x82 = if (x81) {
true
} else {
false
}
x82
}
x83
}
else if (id == 35) {
val x78 = x65 == 'A'
val x84 = if (x78) {
false
} else {
false
}
x84
}
else if (id == 32) {
val x78 = x65 == 'A'
val x85 = if (x78) {
false
} else {
false
}
x85
}
else { throw new RuntimeException("invalid state " + id) }
}
}
class MatchUSD extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id = 86
var i = 0
val n_dec = n-1
while (i < n_dec) {
val x151 = input.charAt(i)
id =
if (id == 120) {
return false
}
else if (id == 115) {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x160 = if (x159) {
120
} else {
return false
}
x160
}
else if (id == 110) {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x162 = if (x159) {
115
} else {
return false
}
x162
}
else if (id == 103) {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x166 = if (x159) {
103
} else {
val x164 = x151 == '.'
val x165 = if (x164) {
110
} else {
return false
}
x165
}
x166
}
else if (id == 131) {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x167 = if (x159) {
103
} else {
return false
}
x167
}
else if (id == 98) {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x173 = if (x159) {
103
} else {
val x169 = x151 == '+'
val x172 = if (x169) {
131
} else {
val x170 = x151 == '-'
val x171 = if (x170) {
131
} else {
return false
}
x171
}
x172
}
x173
}
else if (id == 95) {
val x175 = x151 == ' '
val x176 = if (x175) {
98
} else {
return false
}
x176
}
else if (id == 92) {
val x178 = x151 == 'd'
val x179 = if (x178) {
95
} else {
return false
}
x179
}
else if (id == 89) {
val x181 = x151 == 's'
val x182 = if (x181) {
92
} else {
return false
}
x182
}
else if (id == 86) {
val x184 = x151 == 'u'
val x185 = if (x184) {
89
} else {
return false
}
x185
}
else { throw new RuntimeException("invalid state " + id) }
i += 1
}
val x151 = input.charAt(i)
if (id == 120) {
false
}
else if (id == 115) {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x160 = if (x159) {
true
} else {
false
}
x160
}
else if (id == 110) {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x162 = if (x159) {
false
} else {
false
}
x162
}
else if (id == 103) {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x166 = if (x159) {
false
} else {
val x164 = x151 == '.'
val x165 = if (x164) {
false
} else {
false
}
x165
}
x166
}
else if (id == 131) {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x167 = if (x159) {
false
} else {
false
}
x167
}
else if (id == 98) {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x173 = if (x159) {
false
} else {
val x169 = x151 == '+'
val x172 = if (x169) {
false
} else {
val x170 = x151 == '-'
val x171 = if (x170) {
false
} else {
false
}
x171
}
x172
}
x173
}
else if (id == 95) {
val x175 = x151 == ' '
val x176 = if (x175) {
false
} else {
false
}
x176
}
else if (id == 92) {
val x178 = x151 == 'd'
val x179 = if (x178) {
false
} else {
false
}
x179
}
else if (id == 89) {
val x181 = x151 == 's'
val x182 = if (x181) {
false
} else {
false
}
x182
}
else if (id == 86) {
val x184 = x151 == 'u'
val x185 = if (x184) {
false
} else {
false
}
x185
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
val x191 = input.charAt(i)
id =
if (id == 187) {
return true
}
else { throw new RuntimeException("invalid state " + id) }
i += 1
}
val x191 = input.charAt(i)
if (id == 187) {
true
}
else { throw new RuntimeException("invalid state " + id) }
}
}
class MatchCook extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id = 194
var i = 0
val n_dec = n-1
while (i < n_dec) {
val x216 = input.charAt(i)
id =
if (id == 120) {
return false
}
else if (id == 194) {
val x225 = x216 == 'A'
val x228 = if (x225) {
197
} else {
val x226 = x216 == 'B'
val x227 = if (x226) {
194
} else {
return false
}
x227
}
x228
}
else if (id == 202) {
val x225 = x216 == 'A'
val x228 = if (x225) {
197
} else {
val x226 = x216 == 'B'
val x227 = if (x226) {
194
} else {
return false
}
x227
}
x228
}
else if (id == 197) {
val x225 = x216 == 'A'
val x230 = if (x225) {
197
} else {
val x226 = x216 == 'B'
val x229 = if (x226) {
202
} else {
return false
}
x229
}
x230
}
else { throw new RuntimeException("invalid state " + id) }
i += 1
}
val x216 = input.charAt(i)
if (id == 120) {
false
}
else if (id == 194) {
val x225 = x216 == 'A'
val x228 = if (x225) {
true
} else {
val x226 = x216 == 'B'
val x227 = if (x226) {
true
} else {
false
}
x227
}
x228
}
else if (id == 202) {
val x225 = x216 == 'A'
val x228 = if (x225) {
true
} else {
val x226 = x216 == 'B'
val x227 = if (x226) {
true
} else {
false
}
x227
}
x228
}
else if (id == 197) {
val x225 = x216 == 'A'
val x230 = if (x225) {
true
} else {
val x226 = x216 == 'B'
val x229 = if (x226) {
true
} else {
false
}
x229
}
x230
}
else { throw new RuntimeException("invalid state " + id) }
}
}
