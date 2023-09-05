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

  def setHead[A](xs: List[A], x: A): List[A] =
    xs match
      case Nil        => Nil
      case Cons(h, t) => Cons(x, t)

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

  def append[A](a: List[A], b: List[A]): List[A] =
    a match
      case Nil        => b
      case Cons(h, t) => Cons(h, append(t, b))

  // not constant time
  def init[A](xs: List[A]): List[A] =
    xs match
      case Nil          => Nil
      case Cons(h, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))

  def foldRight[A, B](xs: List[A], acc: B, f: (A, B) => B): B =
    xs match
      case Nil        => acc
      case Cons(h, t) => f(h, foldRight(t, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, _ + _)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1, _ * _)

  def len[A](as: List[A]): Int =
    foldRight(as, 0, (_, y) => 1 + y)
