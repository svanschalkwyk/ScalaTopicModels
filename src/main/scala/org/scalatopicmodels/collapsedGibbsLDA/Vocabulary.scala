package org.scalatopicmodels.collapsedGibbsLDA

import edu.stanford.nlp.process.{CoreLabelTokenFactory, PTBTokenizer}
import java.io.{FileReader, File}
import scala.io.Source
import scala.collection.immutable.HashMap

/**
 * Created by alex on 12/07/14.
 */
object Vocabulary {

  val stopWords = Source.fromURL(getClass.getResource("/stopWords/english_stops_words.txt")).mkString.split("\n").toSet

  def getVocabulary(filePath: String, threshold: Int): HashMap[String, Int] = {

    //var vocabulary: Set[String] = Set.empty
    var vocabulary: HashMap[String, Int] = HashMap.empty

    var wordCounter: HashMap[String, Int] = HashMap.empty

    def countWords(docFile: File) {

      val tokenizer = new PTBTokenizer(new FileReader(docFile), new CoreLabelTokenFactory(), "")

      while (tokenizer.hasNext) {
        val token = tokenizer.next.value().toLowerCase

        if (wordCounter.contains(token)) {
          wordCounter += (token -> (wordCounter(token) + 1))
        }
        else if (!stopWords.contains(token) && token.length>2) {
          wordCounter += (token -> 1)
        }
      }
    }

    new File(filePath).listFiles.toIterator.filter(_.isFile).toList.map(docFile => countWords(docFile))

    for ((w, freq) <- wordCounter) {
      if (freq >= threshold) {
        vocabulary += (w -> vocabulary.size)

      }
    }

    return vocabulary
  }

}
