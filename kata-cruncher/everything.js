throw new Error("A file which uses every syntactic feature of (ES5) JavaScript. Don't run this nonsense...")
var a
var b = 1
var c = 2.0, d = 3e0
let e
let f = undefined
let g = -5, h = -6.0
const i = 7
const j = 8, k = 9
function l() {
  return true
}
function m(n) {
  return n
}
const o = (p) => p
const q = (r, s = 0, ..._) => {
  return r + s
}
if (o(false)) {
  a = 1
} else if (q(3, 1)) {
  b += 2
} else {
  c -= 3
}
switch (a) {
  case 1:
    e = 3
    break
  default:
    f *= 9
}
let t = /I'm a regex!/
class U extends V {
  w
  value = {
    [b]: 3
  }
  constructor(w) {
    super()
    this.w = w
  }
  *[Symbol.iterator]() {
    return this
  }
  next() {
    this.value[b] = null
  }
}
let x = 1 + 2 - 3 * 4 / 5 ** 6 & 7 | 8 ^ 9 << 10 >> 11 >>> 12
let y = a && b || (c ?? d) ? e : f
try {

} catch (e) {

} finally {

}
;
debugger
label: while (x) {
  do {
    ++a
    --b
    c++
    d--
  } while (y)
}
for (let i = 0; i < 3; i++) {
  console.log(i)
}
for (const value of [1, 2, 3]) {
  console.log(value)
}
for (const key in Object) {
  console.log(key)
}
let z = +1 - -2 * !3 / ~4
void typeof z
delete U.w
if (x == 1 && y != z && a === 1 || b !== c && d < e || f <= g || h > i || j >= k
  && "x" in y && z instanceof U) {
  // phew
}
