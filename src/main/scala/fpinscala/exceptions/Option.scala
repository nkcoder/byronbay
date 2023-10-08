package my.playground
package fpinscala.exceptions
import scala.{None as _, Some as _}

// shadow the Option from Scala library
import fpinscala.exceptions.Option.{None, Some}

enum Option[+A]:
  case Some(get: A)
  case None

  // 4.1 implement all the functions of Option (todo)
  def map[B](f: A => B): Option[B] = this match
    case Some(v) => Some(f(v))
    case None    => None

  def getOrElse[B >: A](default: => B): B = this match
    case None    => default
    case Some(v) => v

  def flatMap[B](f: A => Option[B]): Option[B] = this match
    case Some(v) => f(v)
    case None    => None

  def flatMapViaMap[B](f: A => Option[B]): Option[B] =
    map(f).getOrElse(None)

  def orElse[B >: A](ob: => Option[B]): Option[B] = this match
    case None    => ob
    case Some(v) => Some(v)

  def orElseViaMap[B >: A](ob: => Option[B]): Option[B] =
    map(Some(_)).getOrElse(ob)

  def filter(f: A => Boolean): Option[A] = this match
    case Some(v) if f(v) => Some(v)
    case _               => None

  def filterViaMap(f: A => Boolean): Option[A] =
    flatMap(x => if f(x) then Some(x) else None)

object Option:
  def mean(xs: Seq[Double]): Option[Double] =
    if xs.isEmpty then None
    else Some(xs.sum / xs.length)

  // 4.2 implement the `variance` function in terms of flatMap (todo)
  def variance(xs: Seq[Double]): Option[Double] =
    mean(xs).flatMap(m => mean(xs.map(x => math.pow(x - m, 2))))

  def lift[A, B](f: A => B): Option[A] => Option[B]  = _.map(f)
  def lift2[A, B](f: A => B): Option[A] => Option[B] = x => x.map(y => f(y))

  val abs0: Option[Double] => Option[Double] = lift(math.abs)

  // 4.3 write a generic function `map2` that combines two Option values using a binary function
  def map2[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    a.flatMap(a1 => b.map(b1 => f(a1, b1)))

  // 4.4 write a funtion `sequence` that combines a list of Options into one Option containing a list of all the Some values in the original list. (todo)
  def sequence[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((a, acc) => map2(a, acc)(_ :: _))

  def sequence2[A](as: List[Option[A]]): Option[List[A]] =
    as.foldRight[Option[List[A]]](Some(Nil))((oa, acc) => oa.flatMap(a => acc.map(ac => a :: ac)))

  // the following method will traverse the list twice
  def parseInt(as: List[String]): Option[List[Int]] =
    def toIntOption(s: String): Option[Int] =
      try Some(Integer.parseInt(s))
      catch case _: NumberFormatException => None

    sequence(as.map(toIntOption))

  // 4.5 implement the generic function `traverse` that traverses the list only once(todo)
  def traverse[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] =
    as.foldRight[Option[List[B]]](Some(Nil))((a, acc) => map2(f(a), acc)(_ :: _))

  // not so efficient as we traverse the list twice
  def traverseViaSequence[A, B](as: List[A])(f: A => Option[B]): Option[List[B]] = sequence(as.map(f))

  // 4.5 implement `sequence` using `traverse` (the efficient version)
  def sequenceViaTraverse[A](as: List[Option[A]]): Option[List[A]] = traverse(as)(a => a)

  // for comprehension: flatMap ... and final map
  def map2ViaForComprehension[A, B, C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =
    for
      aa <- a
      bb <- b
    yield
      f(aa, bb)
