package org.scalatopicmodels

import breeze.stats.distributions.Multinomial
import breeze.linalg.{sum, DenseVector}

/**
 * Created by alex on 12/07/14.
 */
class collapsedGibbs(docDirectory: String, vocabThreshold: Int, K: Int, alpha: Double, beta: Double) {

  //create corpus instance
  val corpus = new Corpus(docDirectory)

  //if vocabulary is not provided, create one from the documents themselves.
  corpus.getVocabulary(vocabThreshold)
  corpus.initialize(K)

  def gibbsDistribution(word: Word): Multinomial[DenseVector[Double], Int] = {

    var multinomialParams: List[Double] = List.empty

    var wAssignedToTopic = 0
    var wordsInDocWAssignedtoTopic = 0

    //Iterate over topics
    for (topic <- 1 to K) {

      //Total instances of this word assigned to this topic not counting this instance
      if (corpus.wordTopicCounts.contains((word.token, topic))) {
        wAssignedToTopic = corpus.wordTopicCounts((word.token, topic))

        //If this current instance is assigned to this topic, subtract one from count
        if (word.topic == topic) {
          wAssignedToTopic -= 1
        }
      }

      //Total instances of this topic
      var totalAssignedtoTopic = corpus.words.filter(x => x.topic == topic).size
      //If this instance is assigned to this topic, subtract one
      if (word.topic == topic) {
        totalAssignedtoTopic -= 1
      }

      //Total instances assigned to this topic in this instances document not including this instance
      if (corpus.docTopicCounts.contains((word.doc, topic))) {
        wordsInDocWAssignedtoTopic = corpus.docTopicCounts((word.doc, topic))
        //If this instance is assigned to this topic, subtract one
        if (word.topic == topic) {
          wordsInDocWAssignedtoTopic -= 1
        }
      }

      //Total words in this instances document, not including itself
      var totalWordsInDocW = corpus.docSize(word.doc) - 1

      var paramK = ((wAssignedToTopic + beta) / (totalAssignedtoTopic + corpus.vocabulary.size * beta)) * (wordsInDocWAssignedtoTopic + alpha) / (totalWordsInDocW + K * alpha)

      multinomialParams = multinomialParams ++ List(paramK)
    }

    val unnormalized = DenseVector(multinomialParams.toArray)
    val normalizingConstant = sum(unnormalized)
    val normalizedParams = unnormalized :/ normalizingConstant

    Multinomial(normalizedParams)
  }

  def gibbsSample(numIter: Int) {

    for (iter <- 0 to numIter) {
      for (word <- corpus.words) {
        var multinomialDist = gibbsDistribution(word)

        var oldTopic = word.topic
        //reassign word to topic determined by sample
        word.topic = multinomialDist.draw()

        //increment count to due to reassignment to new topic
        corpus.incrementWordTopicCounts(word.token, word.topic)
        corpus.incrementDocTopicCounts(word.doc, word.topic)

        //decrement counts of old topic assignment
        corpus.decrementWordTopicCounts(word.token, oldTopic)
        corpus.decrementDocTopicCounts(word.doc, oldTopic)


      }

    }

  }


}


