import { assert, assertEquals } from 'https://deno.land/std/testing/asserts.ts'
import { turtlePath, isValid } from './turtle.js'

Deno.test({
  name: "isValid()",
  fn: function() {
    assert(isValid('fd 100 lt 90 bk 100 rt 90 pu pd'))
    assert(isValid('forward 100 left 90 back 100 right 45 penup pendown'))
    assert(!isValid('should be false for randomly ordered instructions'))
    assert(!isValid('fd bk lt rt pu pd 100 100'))
    //assert(!isValid('bk 1 2 3 lt 30'))
    // assert(isValid('repeat 4 [ fd 100 rt 90 ]'))
  },
})
Deno.test({
  name: "turtlePath()",
  fn: function() {
    assertEquals(
      turtlePath('fd 100 rt 90 fd 100 rt 90 fd 100 rt 90 fd 100'),
      'M 0 0 l 0 -100 l 100 0 l 0 100 l -100 0'
    )
  },
})

if (import.meta.main) Deno.runTests()
