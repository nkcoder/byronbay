package my.playground
package fpinscala.datastructures

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

  // 3.2 implement the function `tail` to remove the first element of a List (defined above)
  def tail[A](xs: List[A]): List[A] =
    xs match
      case Nil           => sys.error("cannot get tail of an empty list")
      case Cons(_, tail) => tail

  // 3.3 implement the function `setHead` to replace the first element of a List with a different value
  def setHead[A](xs: List[A], x: A): List[A] =
    xs match
      case Nil        => Nil
      case Cons(h, t) => Cons(x, t)

  // 3.4 implement the function `drop` to remove the first n elements from a List (todo)
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
  def drop2[A](xs: List[A], n: Int): List[A] =
    if n <= 0 then xs
    else
      xs match
        case Nil           => Nil
        case Cons(_, tail) => drop2(tail, n - 1)

  // 3.5: implement `dropWhile` which removes elements from the List prefix as long as they match a predicate (todo)
  @tailrec
  def dropWhile[A](xs: List[A], f: A => Boolean): List[A] =
    xs match
      case Cons(head, tail) if f(head) => dropWhile(tail, f)
      case _                           => Nil

  def append[A](a: List[A], b: List[A]): List[A] =
    a match
      case Nil        => b
      case Cons(h, t) => Cons(h, append(t, b))

  // 3.6: implement a function `init` that returns a List consisting all but the last element in constant time.
  def init[A](xs: List[A]): List[A] =
    xs match
      case Nil          => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t)   => Cons(h, init(t))

  def foldRight[A, B](xs: List[A], acc: B, f: (A, B) => B): B =
    xs match
      case Nil        => acc
      case Cons(h, t) => f(h, foldRight(t, acc, f))

  def sumViaFoldRight(ns: List[Int]): Int =
    foldRight(ns, 0, _ + _)

  def productViaFoldRight(ns: List[Double]): Double =
    foldRight(ns, 1, _ * _)

  // 3.9: compute length of a list using `foldRight`
  def length[A](as: List[A]): Int =
    foldRight(as, 0, (_, y) => 1 + y)

  // 3.10: implement `foldLeft` using recursive (todo)
  @tailrec
  def foldLeft[A, B](as: List[A], acc: B, f: (B, A) => B): B =
    as match
      case Nil        => acc
      case Cons(h, t) => foldLeft(t, f(acc, h), f)

  // 3.11: implement `sum`, `product` and `lenght` of a List using `foldLeft`
  def productViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 1, _ * _)

  def sumViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, _ + _)

  def lengthViaFoldLeft(ns: List[Int]): Int = foldLeft(ns, 0, (acc, _) => 1 + acc)

  // 3.12 write a function to reverse a list using fold (todo)
  def reverse_list[A](xs: List[A]): List[A] = foldLeft(xs, Nil: List[A], (acc, x) => Cons(x, acc))

  // 3.13 write `foldRight` in terms of `foldLeft`, and write `foldLeft` via `foldRight` (todo)
  def foldRightViaFoldLeft[A, B](xs: List[A], acc: B, f: (A, B) => B): B =
    foldLeft(reverse_list(xs), acc, (b, a) => f(a, b))

  // todo
  def foldLeftViaFoldRight[A, B](xs: List[A], acc: B, f: (B, A) => B): B = ???

  // 3.14 implement append in terms of either foldLeft or foldRight (todo)
  def appendViaFold[A](a1: List[A], a2: List[A]): List[A] = foldRight(a1, a2, Cons(_, _))

  // 3.15 write a funnction t concatenate a list of lists into a single list in linear runtime. (todo)
  def concatList[A](xs: List[List[A]]): List[A] = foldRight(xs, Nil: List[A], append)

  // 3.16 write a function to transform a list of integers by adding 1 to each element
  def incrementEach(xs: List[Int]): List[Int] =
    foldRight(xs, Nil: List[Int], (i, acc) => Cons(i + 1, acc))

  // 3.17 write a function that turns each value in a List[Double] into String (use d.toString)
  def doubleToString(xs: List[Double]): List[String] =
    foldRight(xs, Nil: List[String], (x, acc) => Cons(x.toString, acc))

  // 3.18 write a function `map` that generalizes modifying each element in a list while maintaining the structure of the list
  def map[A, B](xs: List[A], f: A => B): List[B] =
    foldRight(xs, Nil: List[B], (x, acc) => Cons(f(x), acc))

  // 3.19 write a function `filter` that removes elements from a list unless they satisfy a given predicate
  def filter[A, B](as: List[A], f: A => Boolean): List[A] =
    as match
      case Cons(h, t) if f(h) => Cons(h, filter(t, f))
      case Cons(_, t)         => filter(t, f)
      case _                  => Nil

  // todo
  def filter2[A, B](xs: List[A], f: A => Boolean): List[A] =
    foldRight(xs, Nil: List[A], (x, acc) => if f(x) then Cons(x, acc) else acc)

  // 3.20 write a function `flatMap` that works like `map` except that the function given will return a list instead of single result
  def flatMap[A, B](as: List[A], f: A => List[B]): List[B] =
    foldRight(as, Nil: List[B], (a, acc) => appendViaFold(f(a), acc))

  // 3.21 use `flatMap` to implement filter
  def filterViaFlatMap[A](as: List[A], f: A => Boolean): List[A] =
    flatMap(as, a => if f(a) then List(a) else Nil)

  // 3.22 write a function that accepts two lists and constructs a new list by adding corresponding elements (todo)
  def addIntListPair(a1: List[Int], a2: List[Int]): List[Int] =
    (a1, a2) match
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(h1 + h2, addIntListPair(t1, t2))

  // 3.23 generalize the function `addListPair` so that it's not specific to integers or addition
  def zipWith[A, B, C](a: List[A], b: List[B], f: (A, B) => C): List[C] =
    (a, b) match
      case (Nil, _)                     => Nil
      case (_, Nil)                     => Nil
      case (Cons(h1, t1), Cons(h2, t2)) => Cons(f(h1, h2), zipWith(t1, t2, f))

  // 3.24 implement `hasSubsequence` to check whether a List contains another List as a subsequence
  @tailrec
  def hasSubsequence[A](sup: List[A], sub: List[A]): Boolean =
    @tailrec
    def startsWith[A](xs: List[A], prefix: List[A]): Boolean =
      (xs, prefix) match
        case (_, Nil)                                 => true
        case (Cons(h1, t1), Cons(h2, t2)) if h1 == h2 => startsWith(t1, t2)
        case _                                        => false

    sup match
      case Nil                       => sub == Nil
      case _ if startsWith(sup, sub) => true
      case Cons(_, t)                => hasSubsequence(t, sub)
