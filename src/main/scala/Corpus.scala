import java.io.{FileReader, File}
import scala.collection.immutable.HashMap
import scala.collection.mutable.ListBuffer
import scala.util.Random
import edu.stanford.nlp.process.{CoreLabelTokenFactory, PTBTokenizer}

/**
 * Created by alex on 24/05/14.
 */
class Corpus(docDirectory: String) {

  var words: ListBuffer[Word] = ListBuffer.empty
  var docTopicCounts: HashMap[(Int, Int), Int] = HashMap.empty
  var wordTopicCounts: HashMap[(String, Int), Int] = HashMap.empty
  var vocabulary: Set[String] = Set.empty

  def getVocabulary(minCountThreshold: Int) {
    vocabulary = Vocabulary.getVocabulary(docDirectory, minCountThreshold)
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

    def docProcessor(docFile: File) = {

      val randomTopicGenerator = new Random
      docIndex += 1

      val tokenizer = new PTBTokenizer(new FileReader(docFile), new CoreLabelTokenFactory(), "")

      while (tokenizer.hasNext) {
        val token = tokenizer.next.value().toLowerCase()

        val topic = randomTopicGenerator.nextInt(numTopics)

        if (vocabulary.contains(token)) {
          //Assign the word to a random topic
          words += Word(token, docIndex, topic)

          //update docTopic and wordTopic counters
          incrementDocTopicCounts(docIndex, topic)
          incrementWordTopicCounts(token, topic)
        }

      }
    }

    new File(docDirectory).listFiles.toIterator.filter(_.isFile).toList.map(docFile => docProcessor(docFile))
  }

}

