package my.playground
package catscore.typeclass.functor

import java.security.MessageDigest

import cats.Functor

/**
 * Functor: is a type class that abstracts over type constructors that can be map'ed over. Examples of such type
 * constructors are List, Option, and Future.
 *
 * [[https://typelevel.org/cats/typeclasses/functor.html]]
 */

case class Secret[A](value: A) {
  private def hashed: String = {
    val bytes  = value.toString.getBytes
    val digest = MessageDigest.getInstance("SHA-256").digest(bytes)
    digest.map("%02x".format(_)).mkString
  }

  override def toString: String = s"Secret(${hashed})"
}

object Secret {
  given secretFunctor: Functor[Secret] with {
    override def map[A, B](fa: Secret[A])(f: A => B): Secret[B] = Secret(f(fa.value))
  }

  given optionFunctor: Functor[Option] with {
    override def map[A, B](fa: Option[A])(f: A => B): Option[B] = fa.map(f)
  }

  given listFunctor: Functor[List] with {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }

}
