package my.playground
package fpsimplified.todolist

import java.io.FileWriter

import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

import fpsimplified.todolist.IOHelper.{promptUser, readInput, showHelp}

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



