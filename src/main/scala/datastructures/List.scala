package my.playground
package datastructures

import scala.annotation.tailrec

enum List[+A]:
  case Nil
  case Cons(head: A, tail: List[A])

object List:
  def apply[A](as: A*): List[A] =
    if as.isEmpty then Nil
    else Cons(as.head, apply(as.tail*))

  def sum(ints: List[Int]): Int =
    ints match
      case Nil         => 0
      case Cons(x, xs) => x + sum(xs)

  def product(doubles: List[Double]): Double =
    doubles match
      case Nil          => 1.0
      case Cons(0.0, _) => 0.0
      case Cons(x, xs)  => x * product(xs)

  def tail[A](xs: List[A]): List[A] =
    xs match
      case Nil           => sys.error("cannot get tail of an empty list")
      case Cons(_, tail) => tail

  def drop[A](xs: List[A], n: Int): List[A] =
    @tailrec
    def go(as: List[A], n: Int): List[A] =
      if n == 0 then as
      else go(tail(as), n - 1)

    def tail[A](xs: List[A]): List[A] =
      xs match
        case Nil           => Nil
        case Cons(_, tail) => tail

    go(xs, n)

  @tailrec
  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] =
    xs match
      case Nil                         => Nil
      case Cons(head, _) if !f(head)   => xs
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
