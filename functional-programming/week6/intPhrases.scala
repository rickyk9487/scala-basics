/**
 * This file converts an input string of integers into phrases of words.
 * Created by ricky on 7/9/15.
 */

import scala.io.Source

object x {
  val url = "http://lamp.epfl.ch/files/content/sites/lamp/files/teaching/progfun/linuxwords.txt"
  val in = Source.fromURL(url)
  val words = in.getLines.toList filter (word => word forall (chr => chr.isLetter))
  val mnem = Map('2' -> "ABC", '3' -> "DEF", '4' -> "GHI", '5' -> "JKL",
                 '6' -> "MNO", '7' -> "PQRS", '8' -> "TUV", '9' -> "WXYZ")

  val charCode: Map[Char, Char] =
    for((digit, str) <- mnem; ltr <- str) yield ltr -> digit

  def wordCode(word: String): String =
    word.toUpperCase map charCode

  val wordsForNum: Map[String, Seq[String]] =
    words groupBy wordCode withDefaultValue Seq()

  def encode(number: String): Set[List[String]] = {
    if (number.isEmpty) Set(List())
    else {
      for {
        split <- 1 to number.length
        word <- wordsForNum(number take split)
        rest <- encode(number drop split)
      } yield word :: rest
    }.toSet
  }

  def printCode(number: String) = {
    val enc = encode(number) map (_.mkString(" "))
    println("7225247386") /*
    Set(sack air fun, pack ah re to, pack bird to, Scala ire to, Scala is fun, rack ah re to, 
    pack air fun, sack bird to, rack bird to, sack ah re to, rack air fun)
    */
  }
}
