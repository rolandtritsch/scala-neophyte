package neophyte.part04

import scala.io.Source

object Part04 {
  type Word = String
  type Frequency = Int

  def main(args: Array[String]): Unit = {
    require(args.size == 0, s"Usage: ${Part04.getClass.getName.split('$').head}")

    val wordFrequencies = wordHistogram(readResourceAsListOfWords("ulysses.txt"))
    println(wordsWithoutOutliers(wordFrequencies, 10, 30).take(10))
    println(wordsWithoutOutliers2(wordFrequencies, 10, 30).take(10))
    try {
      println(wordsWithoutOutliers3(wordFrequencies).take(10))
    } catch {
      case e: MatchError => println("Ignore expected match error")
    }
    println(wordsWithoutOutliers4(wordFrequencies).take(10))
    println(wordsWithoutOutliers5(wordFrequencies, 3, 25).take(10))
  }

  def readResourceAsListOfWords(resourceName: String): List[Word] = {
    Source.fromResource(resourceName).getLines.flatMap(_.split("[ .,?!]")).filter(_.nonEmpty).toList
  }

  def wordHistogram(words: List[Word]): List[(Word, Frequency)] = {
    words.groupBy(w => w).map {case (k, v) => (k, v.size) }.toList.sortBy(_._2).reverse
  }

  def wordsWithoutOutliers(wordFrequencies: List[(Word, Frequency)], lowerThreshold: Int, upperThreshold: Int): List[String] = {
    wordFrequencies.filter(wf => wf._2 > lowerThreshold && wf._2 < upperThreshold).map(_._1)
  }

  def wordsWithoutOutliers2(wordFrequencies: List[(Word, Frequency)], lowerThreshold: Int, upperThreshold: Int): List[String] = {
    wordFrequencies.filter { case (_, wf) => wf > lowerThreshold && wf < upperThreshold }.map { case (w, _) => w }
  }

  def wordsWithoutOutliers3(wordFrequencies: List[(Word, Frequency)]): List[String] = {
    wordFrequencies.map(pf)
  }

  def wordsWithoutOutliers4(wordFrequencies: List[(Word, Frequency)]): List[String] = {
    //wordFrequencies.collect(pf)
    wordFrequencies.collect(pf2)
  }

  def wordsWithoutOutliers5(wordFrequencies: List[(Word, Frequency)], lowerThreshold: Int, upperThreshold: Int): List[String] = {
    wordFrequencies.collect {case (w, f) if (f > lowerThreshold && f < upperThreshold) => w }
  }

  val pf: PartialFunction[(Word, Frequency), Word] = {
    case (w, f) if(f > 3 && f < 25) => w
  }

  val pf2 = new PartialFunction[(Word, Frequency), Word] {
    override def apply(wordFrequency: (Word, Frequency)): Word = wordFrequency match {
      case (w, f) if(isDefinedAt(wordFrequency)) => w
    }

    override def isDefinedAt(wordFrequency: (Word, Frequency)): Boolean = wordFrequency match {
      case (w, f) if (f > 3 && f < 25) => true
      case _ => false
    }
  }
}