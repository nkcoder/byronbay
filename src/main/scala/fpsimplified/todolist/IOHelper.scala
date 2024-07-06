package my.playground
package fpsimplified.todolist

import scala.io.StdIn
import scala.util.{Failure, Success, Try}

import fpsimplified.todolist.IOHelper.showHelp

object IOHelper:
  def promptUser(): Try[Unit] = Try {
    println("\n(Commands: a \"add task\", d 1, u 1, h, q, v)")
    print("Yo: ")
  }

  def readInput(): Try[String] = Try {
    StdIn.readLine()
  }

  def showHelp(): Try[Unit] = Try {
    val text: String =
      """
        |Possible commands:
        |  a <task>                 - Add a task
        |  d [task number]          - Delete task 1
        |  h                        - Show this help text
        |  v                        - View the tasks
        |  q                        - Quit the program
        """.stripMargin.trim

    println(text)
  }

class InputHandler(db: Database):

  def processInput(input: String): Try[Unit] = Try {
    input match
      case "h" => showHelp()
      case "q" =>
        println("Goodbye!")
        System.exit(0)
      case add if add.startsWith("a ") =>
        handleAdd(add.drop(2))
        handleView()
      case del if del.startsWith("d ") =>
        handleDelete(del.drop(2))
        handleView()
      case "v" => handleView()
      case _   => println(s"Invalid input: $input")
  }

  private def handleAdd(task: String): Try[Unit] = db.insert(task)

  private def handleDelete(taskIdString: String): Try[Unit] = Try {
    val indexToDelete = taskIdString.toInt - 1
    db.delete(indexToDelete).map(_ => ())
  }

  private def handleView(): Try[Unit] = Try {
    val res = db.selectAll()
    res match
      case Success(tasks) =>
        for (task, count) <- tasks.zip(LazyList.from(1))
        do println(s"$count: $task")
      case Failure(e) => println(s"Error: $e")
  }
