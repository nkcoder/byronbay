package my.playground
package datastructures

import datastructures.List.*

import org.scalatest.funsuite.AnyFunSuite

class ListSpec extends AnyFunSuite:
  test(testName = "List should handle it's construction") {
    val emptyList  = Nil
    val stringList = Cons("Hello", Cons("World", Cons("!", Nil)))
    val intList    = List(1, 2, 3)

    assert(stringList == List("Hello", "World", "!"))
    assert(intList == Cons(1, Cons(2, Cons(3, Nil))))
  }
