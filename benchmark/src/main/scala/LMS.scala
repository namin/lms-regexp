class MatchAAB extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id: Int = 0
var i = 0
val n_dec = n-1
while (i < n_dec) {
val x18 = input.charAt(i)
id = id match {
case 0 => {
val x26 = x18 == 'A'
val x27 = if (x26) {
2
} else {
0
}
x27
}
case 1 => {
val x26 = x18 == 'A'
val x30 = if (x26) {
1
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
case 2 => {
val x26 = x18 == 'A'
val x31 = if (x26) {
1
} else {
0
}
x31
}
}
i += 1
}
val x18 = input.charAt(i)
id match {
case 0 => {
val x26 = x18 == 'A'
val x27 = if (x26) {
false
} else {
false
}
x27
}
case 1 => {
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
case 2 => {
val x26 = x18 == 'A'
val x31 = if (x26) {
false
} else {
false
}
x31
}
}
}
}
class MatchAABany extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id: Int = 5
var i = 0
val n_dec = n-1
while (i < n_dec) {
val x65 = input.charAt(i)
id = id match {
case 0 => {
val x78 = x65 == 'A'
val x79 = if (x78) {
return true
} else {
return true
}
x79
}
case 1 => {
val x78 = x65 == 'A'
val x79 = if (x78) {
return true
} else {
return true
}
x79
}
case 2 => {
val x78 = x65 == 'A'
val x80 = if (x78) {
return true
} else {
return true
}
x80
}
case 3 => {
val x78 = x65 == 'A'
val x83 = if (x78) {
3
} else {
val x81 = x65 == 'B'
val x82 = if (x81) {
return true
} else {
5
}
x82
}
x83
}
case 4 => {
val x78 = x65 == 'A'
val x84 = if (x78) {
3
} else {
5
}
x84
}
case 5 => {
val x78 = x65 == 'A'
val x85 = if (x78) {
4
} else {
5
}
x85
}
}
i += 1
}
val x65 = input.charAt(i)
id match {
case 0 => {
val x78 = x65 == 'A'
val x79 = if (x78) {
true
} else {
true
}
x79
}
case 1 => {
val x78 = x65 == 'A'
val x79 = if (x78) {
true
} else {
true
}
x79
}
case 2 => {
val x78 = x65 == 'A'
val x80 = if (x78) {
true
} else {
true
}
x80
}
case 3 => {
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
case 4 => {
val x78 = x65 == 'A'
val x84 = if (x78) {
false
} else {
false
}
x84
}
case 5 => {
val x78 = x65 == 'A'
val x85 = if (x78) {
false
} else {
false
}
x85
}
}
}
}
class MatchUSD extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id: Int = 9
var i = 0
val n_dec = n-1
while (i < n_dec) {
val x151 = input.charAt(i)
id = id match {
case 0 => {
return false
}
case 1 => {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x160 = if (x159) {
0
} else {
return false
}
x160
}
case 2 => {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x162 = if (x159) {
1
} else {
return false
}
x162
}
case 3 => {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x166 = if (x159) {
3
} else {
val x164 = x151 == '.'
val x165 = if (x164) {
2
} else {
return false
}
x165
}
x166
}
case 4 => {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x167 = if (x159) {
3
} else {
return false
}
x167
}
case 5 => {
val x157 = '0' <= x151
val x158 = x151 <= '9'
val x159 = x157 && x158
val x173 = if (x159) {
3
} else {
val x169 = x151 == '+'
val x172 = if (x169) {
4
} else {
val x170 = x151 == '-'
val x171 = if (x170) {
4
} else {
return false
}
x171
}
x172
}
x173
}
case 6 => {
val x175 = x151 == ' '
val x176 = if (x175) {
5
} else {
return false
}
x176
}
case 7 => {
val x178 = x151 == 'd'
val x179 = if (x178) {
6
} else {
return false
}
x179
}
case 8 => {
val x181 = x151 == 's'
val x182 = if (x181) {
7
} else {
return false
}
x182
}
case 9 => {
val x184 = x151 == 'u'
val x185 = if (x184) {
8
} else {
return false
}
x185
}
}
i += 1
}
val x151 = input.charAt(i)
id match {
case 0 => {
false
}
case 1 => {
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
case 2 => {
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
case 3 => {
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
case 4 => {
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
case 5 => {
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
case 6 => {
val x175 = x151 == ' '
val x176 = if (x175) {
false
} else {
false
}
x176
}
case 7 => {
val x178 = x151 == 'd'
val x179 = if (x178) {
false
} else {
false
}
x179
}
case 8 => {
val x181 = x151 == 's'
val x182 = if (x181) {
false
} else {
false
}
x182
}
case 9 => {
val x184 = x151 == 'u'
val x185 = if (x184) {
false
} else {
false
}
x185
}
}
}
}
class MatchAnything extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return true
var id: Int = return true
var i = 0
val n_dec = n-1
while (i < n_dec) {
val x191 = input.charAt(i)
id = id match {
case 0 => {
return true
}
}
i += 1
}
val x191 = input.charAt(i)
id match {
case 0 => {
true
}
}
}
}
class MatchCook extends (String=>Boolean) {
def apply(input: String): Boolean = {
val n = input.length
if (n == 0) return false
var id: Int = 1
var i = 0
val n_dec = n-1
while (i < n_dec) {
val x216 = input.charAt(i)
id = id match {
case 0 => {
return false
}
case 1 => {
val x225 = x216 == 'A'
val x228 = if (x225) {
3
} else {
val x226 = x216 == 'B'
val x227 = if (x226) {
1
} else {
return false
}
x227
}
x228
}
case 2 => {
val x225 = x216 == 'A'
val x228 = if (x225) {
3
} else {
val x226 = x216 == 'B'
val x227 = if (x226) {
1
} else {
return false
}
x227
}
x228
}
case 3 => {
val x225 = x216 == 'A'
val x230 = if (x225) {
3
} else {
val x226 = x216 == 'B'
val x229 = if (x226) {
2
} else {
return false
}
x229
}
x230
}
}
i += 1
}
val x216 = input.charAt(i)
id match {
case 0 => {
false
}
case 1 => {
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
case 2 => {
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
case 3 => {
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
}
}
}
