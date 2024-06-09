package my.playground
package fpsimplified.errorhandling

import scala.util.control.Exception.{allCatch, catching}
import scala.util.Try

object ErrorHandling {
  // Option
  def makeIntWithOption(s: String): Option[Int] =
    try Some(s.toInt)
    catch case _ => None

  def makeIntWithOption2(s: String): Option[Int] = allCatch.opt(s.toInt)
  
  def makeIntWithOption3(s: String): Option[Int] = catching(classOf[NumberFormatException]).opt(s.toInt)

  // Try
  def makeIntWithTry(s: String): Try[Int] = Try(s.toInt)

  def makeIntWithTry2(s: String): Try[Int] = allCatch.withTry(s.toInt)

  // Either
  def makeIntWithEither(s: String): Either[Throwable, Int] =
    try Right(s.toInt)
    catch case e: Exception => Left(e)

  def makeIntWithEither2(s: String): Either[Throwable, Int] = allCatch.either(s.toInt)
}
