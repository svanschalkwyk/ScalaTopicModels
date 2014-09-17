import org.scalatest._
import scala.collection.immutable.HashMap
import com.topic.models.corpus.CollapsedLDACorpus
import breeze.linalg.{Axis, sum, DenseMatrix, DenseVector}

class CorpusSpec extends FlatSpec with Matchers {

  "CollapsedLDACorpus" should "generate the correct output" in {
    val testVocab: HashMap[String, Int] = HashMap(("i" -> 0), ("went" -> 1), ("store" -> 2), ("to" -> 3), ("the" -> 4))
    val testNumTopics = 2

    val testDoc1 = "I went to the store. The store is where I went. went!"
    val testDoc2 = "I also went to the book store.  I went a lot of places."

    val testInstance = new CollapsedLDACorpus(testVocab, testNumTopics, List("foo", "bar"))

    testInstance.processDoc(testDoc1)
    testInstance.processDoc(testDoc2)

    val correctTWColSum: DenseMatrix[Double] = DenseMatrix(Array(4.0, 5.0, 3.0, 2.0, 3.0))
    val actualTWColSum: DenseMatrix[Double] = sum(testInstance.topicWordMatrix, Axis._0)

    actualTWColSum should equal(correctTWColSum)

    val correctDTColSum: DenseVector[Double] = DenseVector(Array(10.0, 7.0))
    val actualDTColSum: DenseVector[Double] = sum(testInstance.docTopicMatrix, Axis._1)

    actualDTColSum should equal(correctDTColSum)

  }

}
