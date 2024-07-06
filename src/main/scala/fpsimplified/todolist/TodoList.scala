package my.playground
package fpsimplified.todolist

import scala.util.Try

import fpsimplified.todolist.IOHelper.{promptUser, readInput}

/**
 * A simple task list app: add, remove, and list tasks.
 * Rules:
 * - Immutable variables
 * - Immutable data structures
 * - Pure functions
 * - Expression-oriented programming
 * - Functional error-handling
 * - Any function that interacts with the outside world should return a Try or an Either.
 */
object TodoList {
  @main
  def main(): Try[Unit] = {
    val db           = Database("tasks.txt")
    val inputHandler = InputHandler(db)

    def mainLoop(): Try[Unit] =
      for {
        _     <- promptUser()
        input <- readInput()
        _     <- inputHandler.processInput(input)
        _     <- mainLoop()
      } yield ()

    mainLoop()
  }
}
