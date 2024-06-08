package my.playground
package fpsimplified.wordcount

import org.scalatest.funsuite.AnyFunSuite

import scala.collection.immutable.VectorMap

class WordCountSpec extends AnyFunSuite:
  test(testName = "should count the number of occurrences of each word in the document") {
    val document =
      """"Functional programming is
         | a declarative programming paradigm style!
         | """.stripMargin
    val wordCountResult = WordCount.countWords(document)

    assert(
      wordCountResult == VectorMap(
        "programming" -> 2,
        "functional"  -> 1,
        "is"          -> 1,
        "a"           -> 1,
        "declarative" -> 1,
        "paradigm"    -> 1,
        "style"       -> 1
      )
    )
  }
