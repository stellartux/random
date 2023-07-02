import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { from, to } from './main.js'

const trim = ([s]) => s.trim?.() ?? s
const input = from.JavaScript(`
describe("the function", function () {
  it("should do the thing", () => {
    assert.strictEqual(myFunction(2, 1), 3)
  })
})`)

Deno.test('Simple kata - Julia', function () {
  assertEquals(to.Julia(input), trim`
facts("the function") do
    context("should do the thing") do
        @fact myfunction(2, 1) --> 3
    end
end
`)
})

Deno.test('Simple kata - Lua', function () {
  assertEquals(to.Lua(input), trim`
describe("the function", function () 
    it("should do the thing", function () 
        assert.equal(3, myFunction(2, 1))
    end)
end)
`)
})

Deno.test('Simple kata - Common Lisp', function () {
  assertEquals(to.CommonLisp(input), trim`
(deftest the-function
 (testing "should do the thing"
  (ok (= (my-function 2 1) 3)))
`)
})

Deno.test('Simple kata - JavaScript', function () {
  // converts to a consistent formatting
  assertEquals(to.JavaScript(input), trim`
describe('the function', function () {
  it('should do the thing', () => {
    assert.strictEqual(myFunction(2, 1), 3)
  })
})
`)
})

const consoleLog = from.JavaScript('console.log("woo!")')

Deno.test('console.log - JavaScript', () => {
  assertEquals(to.JavaScript(consoleLog), "console.log('woo!')")
})
Deno.test('console.log - Julia', () => {
  assertEquals(to.Julia(consoleLog), 'print("woo!")')
})
Deno.test('console.log - Common Lisp', () => {
  assertEquals(to.CommonLisp(consoleLog), '(print "woo!")')
})
Deno.test('console.log - Lua', () => {
  assertEquals(to.Lua(consoleLog), 'print("woo!")')
})

const forLoop = from.JavaScript('for (let i = 0; i <= 10; i++) {}')

Deno.test('for loop - Julia', () => {
  assertEquals(to.Julia(forLoop), trim`
for i = 0:10

end`)
})

Deno.test('for loop - Lua', () => {
  assertEquals(to.Lua(forLoop), trim`
for i = 0, 10 do

end`)
})

const forLoop2 = from.JavaScript('for (let i = 0; i < 10; i += 2) {}')

Deno.test('for loop 2 - Julia', () => {
  assertEquals(to.Julia(forLoop2), trim`
for i = 0:2:9

end`)
})

Deno.test('for loop 2 - Lua', () => {
  assertEquals(to.Lua(forLoop2), trim`
for i = 0, 9, 2 do

end`)
})

const forLoop3 = from.JavaScript('for (let i = 10; i >= 0; i--) {}')

Deno.test('for loop 3 - Julia', () => {
  assertEquals(to.Julia(forLoop3), trim`
for i = 10:-1:0

end`)
})

Deno.test('for loop 3 - Lua', () => {
  assertEquals(to.Lua(forLoop3), trim`
for i = 10, 0, -1 do

end`)
})
