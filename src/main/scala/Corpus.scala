import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Random
import java.io.File
import java.io.FileReader
import edu.stanford.nlp.process.PTBTokenizer
import edu.stanford.nlp.process.CoreLabelTokenFactory
import scala.io.Source

/**
 * Created by alex on 24/05/14.
 */
class Corpus(docs: List[String]) {

  var words: ListBuffer[Word] = ListBuffer.empty
  var docTopicCounts: HashMap[(Int, Int), Int] = HashMap.empty
  var wordTopicCounts: HashMap[(String, Int), Int] = HashMap.empty
  var vocabulary: Set[String] = Set.empty
  val stopWords=Source.fromURL(getClass.getResource("/english_stops_words.txt")).mkString.split("\n").toSet


  def getVocabulary(filePath: String, threshold: Int) {

    var wordCounter = HashMap[String, Int]()

    def countWords(docFile: File) {

      val tokenizer = new PTBTokenizer(new FileReader(docFile), new CoreLabelTokenFactory(), "")

      while (tokenizer.hasNext) {
        val token = tokenizer.next.value().toLowerCase

        if (wordCounter.contains(token)) {
          wordCounter += (token -> (wordCounter(token) + 1))
        }
        else if(!stopWords.contains(token)) {
          wordCounter += (token -> 1)
        }
      }
    }

    new File(filePath).listFiles.toIterator.filter(_.isFile).toList.map(docFile => countWords(docFile))

    for ((w, freq) <- wordCounter) {
      if (freq > threshold) {
        vocabulary += w
      }
    }
  }


  def incrementDocTopicCounts(doc: Int, topic: Int) {
    if (docTopicCounts.contains((doc, topic))) {
      docTopicCounts += ((doc, topic) -> (docTopicCounts((doc, topic)) + 1))
    }
    else {
      docTopicCounts += ((doc, topic) -> 1)
    }
  }


  def decrementDocTopicCounts(doc: Int, topic: Int) {
    if (docTopicCounts.contains((doc, topic))) {
      docTopicCounts += ((doc, topic) -> (docTopicCounts((doc, topic)) - 1))
    }
  }

  def incrementWordTopicCounts(word: String, topic: Int) {
    if (wordTopicCounts.contains((word, topic))) {
      wordTopicCounts += ((word, topic) -> (wordTopicCounts((word, topic)) + 1))
    }
    else {
      wordTopicCounts += ((word, topic) -> 1)
    }
  }

  def decrementWordTopicCounts(word: String, topic: Int) {
    if (wordTopicCounts.contains((word, topic))) {
      wordTopicCounts += ((word, topic) -> (wordTopicCounts((word, topic)) - 1))
    }
  }

  def initialize(numTopics: Int) = {
    var docIndex = 0

    def docProcessor(doc: String) = {
      val randomTopicGenerator = new Random
      docIndex += 1
      //remove all punctuation at end of word
      for (word <- doc.replaceAll("(\\w+)\\p{Punct}(\\s|$)", "$1$2").split(" ")) {

        val topic = randomTopicGenerator.nextInt(numTopics)

        //Assign the word to a random topic
        words += Word(word, docIndex, topic)

        //update docTopic and wordTopic counters
        incrementDocTopicCounts(docIndex, topic)
        incrementWordTopicCounts(word, topic)
      }
    }
    docs.map(doc => docProcessor(doc))
  }


}

