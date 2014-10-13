package com.topic.models.vocabulary

import scala.collection.immutable.HashMap
import java.io.File
import scala.io.Source
import com.topic.models.tokenizer.StanfordTokenizer
import scala.util.matching.Regex

object CountVocab {
  def apply(fp: String, mc: Int) = new CountVocab(fp, mc)
}

/**
 * Get a vocabulary for the corpus.
 * @param filePath the location of the document files.
 * @param minCount the minimum word frequency for a word to appear in the vocabulary.
 */
class CountVocab(filePath: String, minCount: Int) extends Vocabulary {

  val stopWords = Source.fromURL(getClass.getResource("/english_stops_words.txt")).mkString.split("\n").toSet

  def getVocabulary: HashMap[String, Int] = {

    var vocabulary: HashMap[String, Int] = HashMap.empty

    var wordCounter: HashMap[String, Int] = HashMap.empty

    def countWords(docFile: File) {

      val tokens = StanfordTokenizer.tokenizeFile(docFile)

      for (token <- tokens) {

        if (wordCounter.contains(token)) {
          wordCounter += (token -> (wordCounter(token) + 1))
        }
        else if (!stopWords.contains(token) && !token.matches("\\p{Punct}") && token.length > 2 && token.length < 15 && token != "-lrb-" && token != "-rrb-") {
          wordCounter += (token -> 1)
        }
      }
    }

    new File(filePath).listFiles.toIterator.filter(_.isFile).toList.map(docFile => countWords(docFile))

    for ((w, freq) <- wordCounter) {
      if (freq >= minCount) {
        vocabulary += (w -> vocabulary.size)
      }
    }

    vocabulary
  }
}
