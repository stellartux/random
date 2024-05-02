import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { from, languages } from './kata-cruncher.js'

const trim = ([s]) => s.trim?.() ?? s
const input = from.JavaScript(`
describe("the function", function () {
  it("should do the thing", () => {
    assert.strictEqual(myFunction(2, 1), 3)
  })
})`)

Deno.test('Simple kata - Julia', function () {
  assertEquals(languages.Julia.toCode(input), trim`
facts("the function") do
    context("should do the thing") do
        @fact myfunction(2, 1) --> 3
    end
end
`)
})

Deno.test('Simple kata - Lua', function () {
  assertEquals(languages.Lua.toCode(input), trim`
describe("the function", function ()
    it("should do the thing", function ()
        assert.equal(3, myFunction(2, 1))
    end)
end)
`)
})

Deno.test('Simple kata - Common Lisp', function () {
  assertEquals(languages['Common Lisp'].toCode(input), trim`
(deftest the-function
 (testing "should do the thing"
  (ok (= (my-function 2 1) 3)))
`)
})

Deno.test('Simple kata - JavaScript', function () {
  // converts to a consistent formatting
  assertEquals(languages.JavaScript.toCode(input), trim`
describe('the function', function () {
  it('should do the thing', () => {
    assert.strictEqual(myFunction(2, 1), 3)
  })
})
`)
})

const consoleLog = from.JavaScript('console.log("woo!")')

Deno.test('console.log - Common Lisp', () => {
  assertEquals(languages['Common Lisp'].toCode(consoleLog), '(displayln "woo!")')
})
Deno.test('console.log - JavaScript', () => {
  assertEquals(languages.JavaScript.toCode(consoleLog), "console.log('woo!')")
})
Deno.test('console.log - Julia', () => {
  assertEquals(languages.Julia.toCode(consoleLog), 'println("woo!")')
})
Deno.test('console.log - Lua', () => {
  assertEquals(languages.Lua.toCode(consoleLog), 'print("woo!")')
})

const forLoop = from.JavaScript('for (let i = 0; i <= 10; i++) {}')

Deno.test('for loop - Julia', () => {
  assertEquals(languages.Julia.toCode(forLoop), trim`
for i = 0:10

end`)
})

Deno.test('for loop - Lua', () => {
  assertEquals(languages.Lua.toCode(forLoop), trim`
for i = 0, 10 do

end`)
})

const forLoop2 = from.JavaScript('for (let i = 0; i < 10; i += 2) {}')

Deno.test('for loop 2 - Julia', () => {
  assertEquals(languages.Julia.toCode(forLoop2), trim`
for i = 0:2:9

end`)
})

Deno.test('for loop 2 - Lua', () => {
  assertEquals(languages.Lua.toCode(forLoop2), trim`
for i = 0, 9, 2 do

end`)
})

const forLoop3 = from.JavaScript('for (let i = 10; i >= 0; i--) {}')

Deno.test('for loop 3 - Julia', () => {
  assertEquals(languages.Julia.toCode(forLoop3), trim`
for i = 10:-1:0

end`)
})

Deno.test('for loop 3 - Lua', () => {
  assertEquals(languages.Lua.toCode(forLoop3), trim`
for i = 10, 0, -1 do

end`)
})

const divOrToZero = from.JavaScript('-5 / 2 | 0')         //= -2
const divToZero = from.JavaScript('Math.trunc(-5 / 2)')   //= -2
const divToNegInf = from.JavaScript('Math.floor(-5 / 2)') //= -3

Deno.test('Integer division - Common Lisp', () => {
  assertEquals(languages['Common Lisp'].toCode(divOrToZero), '(truncate -5 2)')
  assertEquals(languages['Common Lisp'].toCode(divToZero), '(truncate -5 2)')
  assertEquals(languages['Common Lisp'].toCode(divToNegInf), '(floor -5 2)')
})

Deno.test('Integer division - Julia', () => {
  assertEquals(languages.Julia.toCode(divOrToZero), '-5 ÷ 2')
  assertEquals(languages.Julia.toCode(divToZero), 'div(-5, 2)')
  assertEquals(languages.Julia.toCode(divToNegInf), 'fld(-5, 2)')
})

Deno.test('Integer division - Lua', () => {
  // assertEquals(languages.Lua.toCode(divOrToZero), '-5 // 2')
  // assertEquals(languages.Lua.toCode(divToZero), '-5 // 2')
  assertEquals(languages.Lua.toCode(divToNegInf), '-5 // 2')
})

Deno.test('Integer division - Nim', () => {
  assertEquals(languages.Nim.toCode(divOrToZero), '-5 div 2')
  assertEquals(languages.Nim.toCode(divToZero), '-5 div 2')
  // assertEquals(languages.Nim.toCode(divToNegInf), '-5 div 2')
})

Deno.test('Integer division - Prolog', () => {
  assertEquals(languages.Prolog.toCode(divOrToZero), ':- -5 // 2.')
  assertEquals(languages.Prolog.toCode(divToZero), ':- -5 // 2.')
  assertEquals(languages.Prolog.toCode(divToNegInf), ':- -5 div 2.')
})

Deno.test('Integer division - Python', () => {
  // assertEquals(languages.Python.toCode(divOrToZero), '-5 // 2')
  // assertEquals(languages.Python.toCode(divToZero), '-5 // 2')
  assertEquals(languages.Python.toCode(divToNegInf), '-5 // 2')
})

Deno.test('Integer division - Racket', () => {
  assertEquals(languages.Racket.toCode(divOrToZero), '(quotient -5 2)')
  assertEquals(languages.Racket.toCode(divToZero), '(quotient -5 2)')
  assertEquals(languages.Racket.toCode(divToNegInf), '(floor-quotient -5 2)')
})

Deno.test('Integer division - Ruby', () => {
  assertEquals(languages.Ruby.toCode(divOrToZero), '(-5 / 2).floor')
  assertEquals(languages.Ruby.toCode(divToZero), '(-5 / 2).floor')
  assertEquals(languages.Ruby.toCode(divToNegInf), '-5.div(2)')
})

Deno.test('Necessary semicolon - JavaScript', () => {
  const code = from.JavaScript('let a, b\nconst c = [1, 2, 3];\n[a, b] = c')
  assertEquals(languages.JavaScript.toCode(code), 'let a, b\nconst c = [1, 2, 3]\n;[a, b] = c')
})
