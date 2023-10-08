package my.playground
package fpinscala.exceptions

import fpinscala.exceptions.Either.{Left, Right}

import scala.util.control.NonFatal

enum Either[+E, +A]:
  case Left(value: E)
  case Right(value: A)

  def isLeft: Boolean = this match
    case Left(_) => true
    case _       => false

  def isRight: Boolean = !isLeft

  // 4.6 implement versions of `map`, `flatMap`, `orElse` and `map2` on Either that operate on the Right value
  // pay attention to the signature of the functions (todo)
  def map[B](f: A => B): Either[E, B] = this match
    case Left(e)  => Left(e)
    case Right(v) => Right(f(v))

  def flatMap[EE >: E, B](f: A => Either[EE, B]): Either[EE, B] = this match
    case Left(e)  => Left(e)
    case Right(v) => f(v)

  def orElse[EE >: E, B >: A](default: => Either[EE, B]): Either[EE, B] = this match
    case Left(e)  => default
    case Right(v) => Right(v)

  def map2[EE >: E, B, C](that: Either[EE, B])(f: (A, B) => C): Either[EE, C] =
    for
      aa <- this
      bb <- that
    yield f(aa, bb)

object Either:
  def mean(xs: Seq[Double]): Either[String, Double] =
    if xs.isEmpty then Left("mean of Empty list")
    else Right(xs.sum / xs.length)

  def safeDiv(x: Int, y: Int): Either[Throwable, Int] = catchNonFatal(x / y)

  def catchNonFatal[A](a: => A): Either[Throwable, A] =
    try Right(a)
    catch case NonFatal(t) => Left(t)

  // 4.7 implement `sequence` and `traverse` for Either (todo)
  def sequence[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    as.foldRight[Either[E, List[A]]](Right(Nil))((a, acc) => a.map2(acc)(_ :: _))

  def traverse[E, A, B](as: List[A])(f: A => Either[E, B]): Either[E, List[B]] =
    as.foldRight[Either[E, List[B]]](Right(Nil))((a, acc) => f(a).map2(acc)(_ :: _))

  def sequenceViaTraverse[E, A](as: List[Either[E, A]]): Either[E, List[A]] =
    traverse(as)(a => a)

  def map2Both[E, A, B, C](a: Either[E, A], b: Either[E, B])(f: (A, B) => C): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(e), Right(_))    => Left(List(e))
      case (Right(_), Left(e))    => Left(List(e))
      case (Left(e1), Left(e2))   => Left(List(e1, e2))

  // `map2Both` is limited because if the left of the Either is already a List
  def map2All[E, A, B, C](a: Either[List[E], A], b: Either[List[E], B])(f: (A, B) => C): Either[List[E], C] =
    (a, b) match
      case (Right(aa), Right(bb)) => Right(f(aa, bb))
      case (Left(e), Right(_))    => Left(e)
      case (Right(_), Left(e))    => Left(e)
      case (Left(e1), Left(e2))   => Left(e1 ++ e2)

  def traverseAll[E, A, B](as: List[A], f: A => Either[List[E], B]): Either[List[E], List[B]] =
    as.foldRight[Either[List[E], List[B]]](Right(Nil))((a, acc) => map2All(f(a), acc)(_ :: _))

  // todo
  def sequenceViaTraverseAll[E, A](as: List[Either[List[E], A]]): Either[List[E], List[A]] =
    traverseAll(as, identity)
