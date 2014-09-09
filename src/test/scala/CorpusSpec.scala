import com.topic.models.models.collapsedGibbs
import org.scalatest._

class CorpusSpec extends FlatSpec with Matchers {

  //val testDoc1=Source.fromURL(getClass.getResource("/testDocs/doc1.txt")).mkString
  //val testDoc2=Source.fromURL(getClass.getResource("/testDocs/doc2.txt")).mkString
  //val testDoc3=Source.fromURL(getClass.getResource("/testDocs/doc3.txt")).mkString

  //val myCorpus = new Corpus("/home/alex/topic_models",1)

  /*
  "docTopicCounts" should "be incremented for the specified document/Topic pair when incrementDocTopic is called on it" in {
    myCorpus.incrementDocTopicCounts(1, 2)
    myCorpus.docTopicCounts(1, 2) should equal(1)
  }

  it should "also decrement the specified document/Topic pair when decrementDocTopic is called on it" in {
    myCorpus.decrementDocTopicCounts(1, 2)
    myCorpus.docTopicCounts(1, 2) should equal(0)
  }

  "wordTopicCounts" should "be incremented for the specified word/Topic pair when incrementWordTopic is called on it" in {
    myCorpus.incrementWordTopicCounts("blah", 2)
    myCorpus.wordTopicCounts("blah", 2) should equal(1)
  }

  it should "also decrement the specified word/Topic pair when decrementWordTopic is called on it" in {
    myCorpus.decrementWordTopicCounts("blah", 2)
    myCorpus.wordTopicCounts("blah", 2) should equal(0)
  }

  it should "not have any stopwords in the vocabulary" in {
    myCorpus.getVocabulary(1)
    myCorpus.vocabulary should not contain ("the")
    //println(myCorpus.vocabulary)
  }

*/

  var myGibbsLDA = new collapsedGibbs("/home/alex/Documents/topic_models_datasets2/", 120, 5, 0.1, 0.1)

  println("doc/topic matrix")
  //println(myGibbsLDA.corpus.getDocTopicMatrix)
  println("topic/word matrix")
  //println(myGibbsLDA.corpus.getTopicWordMatrix)
  myGibbsLDA.inference
  println("doc/topic matrix after sampling")
  //println(myGibbsLDA.corpus.getDocTopicMatrix)
  println("topic/word matrix after sampling")
  //println(myGibbsLDA.corpus.getTopicWordMatrix)
  //myGibbsLDA.getPhi
  //myGibbsLDA.getTheta
  println("")
  //println(myGibbsLDA.corpus.getDocTopicMatrix)
  println("Top 10 words for each topic")
  myGibbsLDA.printTopics(10)
  myGibbsLDA.printTopicProps(2,0.1)






}
