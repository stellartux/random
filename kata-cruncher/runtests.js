import { assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { convert } from './main.js'

const trim = ([s]) => s.trim?.() ?? s
const input = `
describe("the function", function () {
  it("should do the thing", () => {
    assert.strictEqual(myFunction(2, 1), 3)
  })
})`

Deno.test('Simple kata - Julia', function () {
  assertEquals(convert(input, 'Julia'), trim`
facts("the function") do
    context("should do the thing") do
        @fact myfunction(2, 1) --> 3
    end
end
`)
})

Deno.test('Simple kata - Lua', function () {
  assertEquals(convert(input, 'Lua'), trim`
describe("the function", function () 
    it("should do the thing", function () 
        assert.equal(3, myFunction(2, 1))
    end)
end)
`)
})

Deno.test('Simple kata - Common Lisp', function () {
  assertEquals(convert(input, 'Common Lisp'), trim`
(deftest the-function
 (testing "should do the thing"
  (ok (= (my-function 2 1) 3)))
`)
})

Deno.test('Simple kata - JavaScript', function () {
  // converts to a consistent formatting
  assertEquals(convert(input, 'JavaScript'), trim`
describe('the function', function () {
  it('should do the thing', () => {
    assert.strictEqual(myFunction(2, 1), 3)
  })
})
`)
})

const consoleLog = 'console.log("woo!")'

Deno.test('console.log - JavaScript', () => {
  assertEquals(convert(consoleLog, 'JavaScript'), "console.log('woo!')")
})
Deno.test('console.log - Julia', () => {
  assertEquals(convert(consoleLog, 'Julia'), 'print("woo!")')
})
Deno.test('console.log - Common Lisp', () => {
  assertEquals(convert(consoleLog, 'Common Lisp'), '(print "woo!")')
})
Deno.test('console.log - Lua', () => {
  assertEquals(convert(consoleLog, 'Lua'), 'print("woo!")')
})

const forLoop = 'for (let i = 0; i <= 10; i++) {}'

Deno.test('for loop - Julia', () => {
  assertEquals(convert(forLoop, 'Julia'), trim`
for i = 0:10

end`)
})

Deno.test('for loop - Lua', () => {
  assertEquals(convert(forLoop, 'Lua'), trim`
for i = 0, 10 do

end`)
})

const forLoop2 = 'for (let i = 0; i < 10; i += 2) {}'

Deno.test('for loop 2 - Julia', () => {
  assertEquals(convert(forLoop2, 'Julia'), trim`
for i = 0:2:9

end`)
})

Deno.test('for loop 2 - Lua', () => {
  assertEquals(convert(forLoop2, 'Lua'), trim`
for i = 0, 9, 2 do

end`)
})

const forLoop3 = 'for (let i = 10; i >= 0; i--) {}'

Deno.test('for loop 3 - Julia', () => {
  assertEquals(convert(forLoop3, 'Julia'), trim`
for i = 10:-1:0

end`)
})

Deno.test('for loop 3 - Lua', () => {
  assertEquals(convert(forLoop3, 'Lua'), trim`
for i = 10, 0, -1 do

end`)
})
