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

  test(testName = "List should calculate the sum when data is empty") {
    val sumOfEmptyList = sum(Nil)
    assert(sumOfEmptyList == 0)
  }

  test(testName = "List should calculate the sum data is not empty") {
    val sumOfInts = sum(List(34, 90, 45))
    assert(sumOfInts == 169)
  }

  test(testName = "List should calculate the product when data is empty") {
    val sumOfEmptyList = product(Nil)
    assert(sumOfEmptyList == 1.0)
  }

  test(testName = "List should calculate the sum when data contains 0") {
    val sumOfInts = product(List(34, 0, 45))
    assert(sumOfInts == 0.0)
  }

  test(testName = "List should calculate the sum when data doesn't contain 0") {
    val sumOfInts = product(List(5, 10, 3))
    assert(sumOfInts == 150)
  }

  test("tail: should throw error when the list is empty") {
    assertThrows[RuntimeException](tail(Nil))
  }

  test("tail: should return tail when the list is not empty") {
    val tailOfList = tail(List(1, 2, 3, 4))
    assert(tailOfList == List(2, 3, 4))
  }

  test("setHead: should return empty when the list is empty") {
    val newList = setHead(Nil, 10)
    assert(newList == Nil)
  }

  test("setHead: should set head when the list is nonempty") {
    val newList = setHead(List(1, 2, 3), 10)
    assert(newList == List(10, 2, 3))
  }

  test("drop: should remove n elements from the list") {
    val xs = List(1, 2, 3, 4, 5, 6)
    assert(drop(xs, 3) == List(4, 5, 6))
  }

  test("drop: should return empty when removing more elements from the list") {
    val xs = List(1, 2, 3)
    assert(drop(xs, 5) == Nil)
  }

  test("dropWhile: should remove elements from the list unless condition is not met") {
    val xs       = List(2, 4, 6, 7, 8)
    val remained = dropWhile(xs, x => x % 2 == 0)
    assert(remained == List(7, 8))
  }

  test("dropWhile: should return empty when all elements are removed") {
    val xs       = List(10, 25, 38, 19)
    val remained = dropWhile(xs, x => x >= 10)
    assert(remained == Nil)
  }

end ListSpec
