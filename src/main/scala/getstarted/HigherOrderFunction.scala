package my.playground
package getstarted

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

}
