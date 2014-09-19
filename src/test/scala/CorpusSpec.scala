import org.scalatest._
import scala.collection.immutable.HashMap
import com.topic.models.corpus.{StreamingCorpus, CollapsedLDACorpus}
import breeze.linalg.{Axis, sum, DenseMatrix, DenseVector}

class CorpusSpec extends FlatSpec with Matchers {

  "CollapsedLDACorpus" should "generate the correct output" in {
    val testVocab: HashMap[String, Int] = HashMap(("i" -> 0), ("went" -> 1), ("store" -> 2), ("to" -> 3), ("the" -> 4))
    val testNumTopics = 2

    val testInstance = new CollapsedLDACorpus(testVocab, testNumTopics, "src/main/resources/testDocs/")

    testInstance.initialize

    val correctTWColSum: DenseMatrix[Double] = DenseMatrix(Array(23.0, 0.0, 0.0, 29.0, 44.0))
    val actualTWColSum: DenseMatrix[Double] = sum(testInstance.topicWordMatrix, Axis._0)

    actualTWColSum should equal(correctTWColSum)

    val correctDTColSum: DenseVector[Double] = DenseVector(Array(20.0, 39.0, 37.0))
    val actualDTColSum: DenseVector[Double] = sum(testInstance.docTopicMatrix, Axis._1)

    actualDTColSum should equal(correctDTColSum)
  }

  "StreamingCorpus" should "generate the correct output" in {
    val testVocab: HashMap[String, Int] = HashMap(("i" -> 0), ("went" -> 1), ("store" -> 2), ("to" -> 3), ("the" -> 4))
    val batchsize: Int = 2

    val testInstance = new StreamingCorpus(testVocab, batchsize, "src/main/resources/testDocs/")

    testInstance.initialize

    val correctMinibatch = List(List((0, 2), (3, 7), (4, 11)), List((0, 12), (3, 13), (4, 14)))
    val actualMinibatch = testInstance.getNextMiniBatch

    actualMinibatch should equal(correctMinibatch)

  }

}
