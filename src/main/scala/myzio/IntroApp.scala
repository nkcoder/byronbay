package my.playground
package myzio

import scala.io.StdIn

import zio.{Task, UIO, ZIO, ZIOAppDefault}

object IntroApp extends ZIOAppDefault {

  /**
   * Task is the alias type of ZIO[Any, Throwable, A]
   */
  private val username: Task[String] = ZIO.attempt(StdIn.readLine("What's your name? "))

  /**
   * UIO is the alias type of ZIO[Any, Nothing, A]
   */
  private def printName(name: String): UIO[Unit] = ZIO.succeed(println(s"Hello, $name!"))

//  override def run = username.flatMap(printName)

  override def run =
    for
      name <- username
      _    <- printName(name)
    yield ()
}
