package my.playground
package fpsimplified.todolist

import java.io.FileWriter

import scala.io.Source
import scala.util.{Try, Using}

class Database(val dbFileName: String):
  def insert(record: String): Either[Throwable, Unit] = writeToFile(List(record), append = true)

  def selectAll(): Either[Throwable, Seq[String]] =
    val selectResult = Using(Source.fromFile(dbFileName)) { source =>
      val lines = for line <- source.getLines() yield line
      lines.toSeq
    }
    selectResult.toEither

  def delete(indexToDelete: Int): Either[Throwable, Int] =
    for {
      lines         <- selectAll()
      newRows        = lines.zipWithIndex.filterNot(_._2 == indexToDelete).map(_._1)
      numRowsDeleted = lines.size - newRows.size
      _             <- writeToFile(lines = newRows, append = false)
    } yield numRowsDeleted

  private def writeToFile(lines: Seq[String], append: Boolean): Either[Throwable, Unit] =
    val writeResult = Using(FileWriter(dbFileName, append)) { file =>
      for line <- lines do file.write(s"$line\n")
    }
    writeResult.toEither
