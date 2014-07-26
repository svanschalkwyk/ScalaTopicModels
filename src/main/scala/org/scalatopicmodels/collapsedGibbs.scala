package org.scalatopicmodels

import breeze.stats.distributions.Multinomial
import breeze.linalg.{sum, DenseVector}

/**
 * Created by alex on 12/07/14.
 */
class collapsedGibbs(docDirectory: String, vocabThreshold: Int, K: Int, alpha: Double, beta: Double) {


  //create corpus instance
  val corpus = new Corpus(docDirectory, vocabThreshold)

  //Randomly initialize topic assignments
  corpus.initialize(K)

  def gibbsDistribution(word: Word): Multinomial[DenseVector[Double], Int] = {

    var multinomialParams: List[Double] = List.empty

    var wAssignedToTopic = 0.0
    var wordsInDocWAssignedtoTopic = 0.0

    //Iterate over topics
    for (topic <- 0 to K - 1) {

      //Total instances of this word assigned to this topic
      wAssignedToTopic = corpus.topicWordMatrix(topic, corpus.vocabulary(word.token))

      //Total instances of this topic
      var totalAssignedtoTopic = sum(corpus.topicWordMatrix(topic, ::).t)

      //Total instances assigned to this topic in this instance's document
      wordsInDocWAssignedtoTopic = corpus.docTopicMatrix(word.doc, topic)

      //if this word is already assigned the this topic, decrement counts
      if (word.topic == topic) {
        wAssignedToTopic -= 1
        totalAssignedtoTopic -= 1.0
        wordsInDocWAssignedtoTopic -= 1
      }

      //Total words in this instance's document, not including itself
      val totalWordsInDocW: Double = sum(corpus.docTopicMatrix(word.doc, ::).t) - 1.0

      //element of multinomial parameter associated with this topic
      val paramK = ((wAssignedToTopic + beta) / (totalAssignedtoTopic + corpus.vocabulary.size * beta)) * (wordsInDocWAssignedtoTopic + alpha) / (totalWordsInDocW + K * alpha)

      multinomialParams = multinomialParams ++ List(paramK)
    }

    //normalize parameters
    val unnormalized = DenseVector(multinomialParams.toArray)
    val normalizingConstant = sum(unnormalized)
    val normalizedParams = unnormalized :/ normalizingConstant

    Multinomial(normalizedParams)
  }

  def gibbsSample(numIter: Int) {

    for (iter <- 0 to numIter) {
      for (word <- corpus.words) {
        val multinomialDist = gibbsDistribution(word)

        val oldTopic = word.topic

        //reassign word to topic determined by sample
        word.topic = multinomialDist.draw()

        //If topic assignment has changed, must also change count matrices
        if (oldTopic != word.topic) {
          //increment counts to due to reassignment to new topic
          corpus.topicWordMatrix(word.topic, corpus.vocabulary(word.token)) += 1.0
          corpus.docTopicMatrix(word.doc, word.topic) += 1.0

          //decrement counts of old topic assignment that has been changed
          corpus.topicWordMatrix(oldTopic, corpus.vocabulary(word.token)) -= 1.0
          corpus.docTopicMatrix(word.doc, oldTopic) -= 1.0

        }
      }

    }

  }


}


