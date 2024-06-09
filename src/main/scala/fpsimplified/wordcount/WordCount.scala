package my.playground
package fpsimplified.wordcount

import scala.collection.immutable.VectorMap
import scala.io.Source
import scala.util.Try

/**
 * Given a document that’s represented as a single Scala String, count the number of occurrences of each word in that document.
 */

/**
 * FPers like to think of this style of programming as math, or perhaps as creating a blueprint. So rather than being a
 * programmer, you’re like a mathematician or architect, working on equations or the blueprint.
 */
type Word                 = String
type Count                = Int
type MapSortedByValueDesc = VectorMap[Word, Count]

object WordCount:
  /**
   * Count the number of occurrences of each word in the document.
   * @param document The document content.
   * @return
   */
  def countWords(document: String): MapSortedByValueDesc =
    val stringWithoutNewLines    = replaceNewLinesWithBlanks(document)
    val stringWithoutFormatting  = stripFormattingCharacters(stringWithoutNewLines)
    val stringWithoutExtraSpaces = squeezeBlankSpaces(stringWithoutFormatting)
    val listOfWords              = convertStringToListOfWords(stringWithoutExtraSpaces)
    val lowerCaseWords           = lowerCaseAllWords(listOfWords)
    val wordCountMap             = convertWordListToWordCount(lowerCaseWords)
    val sortedMap                = sortMapByHighestValue(wordCountMap)
    sortedMap

  private def replaceNewLinesWithBlanks(s: String): String = s.replaceAll("\n", " ")

  private def stripFormattingCharacters(s: String): String = s.replaceAll("[^a-zA-Z0-9\\s]", " ")

  private def squeezeBlankSpaces(s: String): String = s.replaceAll("\\s+", " ")

  private def convertStringToListOfWords(s: String): Seq[Word] = s.trim.split(" ").toSeq

  private def lowerCaseAllWords(words: Seq[String]): Seq[Word] = words.map(_.toLowerCase)

  private def convertWordListToWordCount(words: Seq[String]): Map[Word, Count] =
    words.groupBy(identity).view.mapValues(_.size).toMap

  private def sortMapByHighestValue(map: Map[Word, Count]): MapSortedByValueDesc =
    VectorMap.from(map.toSeq.sortBy(_._2)(Ordering[Int].reverse))

  private type WordCountMapTry    = Try[MapSortedByValueDesc]
  private type WordCountMapEither = Either[Throwable, MapSortedByValueDesc]

  /**
   * The document content is read from a file and the words are counted.
   *
   * @param fileName The name of the file to read.
   * @return
   */
  def countWordsFromFileUsingTry(fileName: String): WordCountMapTry =
    readFileUsingTry(fileName).map(countWords)

  private def readFileUsingTry(fileName: String): Try[String] = Try {
    val source = Source.fromFile(fileName)
    try source.mkString
    finally source.close()
  }

  def countWordsFromFileUsingEither(fileName: String): WordCountMapEither =
    readFileUsingEither(fileName).map(countWords)

  private def readFileUsingEither(fileName: String): Either[Throwable, String] =
    val source = Source.fromFile(fileName)
    try Right(source.mkString)
    catch case e: Exception => Left(e)
    finally source.close()
