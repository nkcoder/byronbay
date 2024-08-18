package my.playground
package catscore.typeclass.applicative

import cats.Applicative

object MyApplicative {
  given optionApplicative: Applicative[Option] with {
    override def pure[A](x: A): Option[A] = Option(x)

    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = (ff, fa) match {
      case (Some(f), Some(a)) => Some(f(a))
      case _                  => None
    }

    def ap_v2[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      for {
        f <- ff
        a <- fa
      } yield f(a)

    def ap_v3[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      ff.flatMap(f => fa.map(f))

    def ap_v4[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] =
      map2(ff, fa)((f, a) => f(a))
  }

  given listApplicative: Applicative[List] with {
    override def pure[A](x: A): List[A] = List(x)

    override def ap[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      for {
        f <- ff
        a <- fa
      } yield f(a)

    def ap_v2[A, B](ff: List[A => B])(fa: List[A]): List[B] =
      map2(ff, fa)((f, a) => f(a))

  }
}
