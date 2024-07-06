package my.playground
package fpsimplified.todolist

import java.io.FileWriter

import scala.io.Source
import scala.util.{Try, Using}

class Database(val dbFileName: String):
  def insert(record: String): Try[Unit] = writeToFile(List(record), append = true)

  def selectAll(): Try[Seq[String]] =
    Using(Source.fromFile(dbFileName)) { source =>
      val lines = for line <- source.getLines() yield line
      lines.toSeq
    }

  def delete(indexToDelete: Int): Try[Int] =
    for {
      lines         <- selectAll()
      newRows        = lines.zipWithIndex.filterNot(_._2 == indexToDelete).map(_._1)
      numRowsDeleted = lines.size - newRows.size
      _             <- writeToFile(lines = newRows, append = false)
    } yield numRowsDeleted

  private def writeToFile(lines: Seq[String], append: Boolean): Try[Unit] =
    Using(FileWriter(dbFileName, append)) { file =>
      for line <- lines do file.write(s"$line\n")
    }
