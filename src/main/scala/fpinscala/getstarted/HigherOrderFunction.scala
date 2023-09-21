package my.playground
package fpinscala.getstarted

import scala.annotation.tailrec

object HigherOrderFunction {

  def findFirst[A](as: Array[A], p: A => Boolean): Int =
    @tailrec
    def loop(n: Int): Int =
      if n >= as.length then -1
      else if p(as(n)) then n
      else loop(n + 1)

    loop(0)

  def isSorted[A](as: Array[A], gt: (A, A) => Boolean): Boolean =
    @tailrec
    def loop(n: Int): Boolean =
      if n >= as.length - 1 then true
      else if !gt(as(n), as(n + 1)) then false
      else loop(n + 1)

    loop(0)

  def partial1[A, B, C](a: A, f: (A, B) => C): B => C =
    (b: B) => f(a, b)

  // the type of b can be inferred from the context
  def partial1V2[A, B, C](a: A, f: (A, B) => C): B => C =
    b => f(a, b)

  // .curried will return A => B => C
  def partial1V3[A, B, C](a: A, f: (A, B) => C): B => C =
    f.curried(a)

  // (a: A) => ((b: B) => ???)
  def curry[A, B, C](f: (A, B) => C): A => (B => C) =
    (a: A) => ((b: B) => f(a, b))

  def curryV2[A, B, C](f: (A, B) => C): A => (B => C) =
    a => b => f(a, b)

  // (a: A, b: B) => ???
  def unCurry[A, B, C](f: A => B => C): (A, B) => C =
    (a: A, b: B) => f(a)(b)

  def unCurryV2[A, B, C](f: A => B => C): (A, B) => C =
    (a, b) => f(a)(b)

  // (a: A) => ???
  def compose[A, B, C](f: B => C, g: A => B): A => C =
    (a: A) => f(g(a))

  def composeV2[A, B, C](f: B => C, g: A => B): A => C =
    a => f(g(a))
}
