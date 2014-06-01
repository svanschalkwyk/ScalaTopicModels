import org.scalatest._

class CorpusSpec extends FlatSpec with Matchers {

  val testDocs=List("This is my first test document.","I am testing the increment and decrement functions","So these are just random words")
  val myCorpus=new Corpus(testDocs)


  "docTopicCounts" should "be incremented for the specified document/Topic pair when incrementDocTopic is called on it" in {
    myCorpus.incrementDocTopicCounts(1,2)
    myCorpus.docTopicCounts(1,2) should equal(1)
  }

  it should "also decrement the specified document/Topic pair when decrementDocTopic is called on it" in {
    myCorpus.decrementDocTopicCounts(1,2)
    myCorpus.docTopicCounts(1,2) should equal(0)
  }

  "wordTopicCounts" should "be incremented for the specified word/Topic pair when incrementWordTopic is called on it" in {
    myCorpus.incrementWordTopicCounts("blah",2)
    myCorpus.wordTopicCounts("blah",2) should equal(1)
  }

  it should "also decrement the specified word/Topic pair when decrementWordTopic is called on it" in {
    myCorpus.decrementWordTopicCounts("blah",2)
    myCorpus.wordTopicCounts("blah",2) should equal(0)
  }
}
