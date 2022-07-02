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
