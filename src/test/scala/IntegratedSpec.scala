import com.topic.models.corpus.StreamingCorpus
import com.topic.models.models.OnlineLDA
import com.topic.models.vocabulary.CountVocab


object IntegratedSpec extends App {

  val docDirectory = "NIPS_dataset/"

  val testVocab = CountVocab(docDirectory, 10).getVocabulary

  val testCorpus = new StreamingCorpus(testVocab, 5, docDirectory)

  val oldaTest = new OnlineLDA(testCorpus, 5, 0.5, 500)

  oldaTest.inference()

  oldaTest.printTopics(10)
}
