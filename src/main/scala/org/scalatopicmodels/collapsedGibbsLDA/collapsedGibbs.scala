package org.scalatopicmodels.collapsedGibbsLDA

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
      wAssignedToTopic = corpus.getTopicWord(topic, corpus.vocabulary(word.token))

      //Total instances of this topic
      var totalAssignedtoTopic = sum(corpus.getTopicWordRow(topic))

      //Total instances assigned to this topic in this instance's document
      wordsInDocWAssignedtoTopic = corpus.getDocTopic(word.doc, topic)

      //if this word is already assigned the this topic, decrement counts
      if (word.topic == topic) {
        wAssignedToTopic -= 1
        totalAssignedtoTopic -= 1.0
        wordsInDocWAssignedtoTopic -= 1
      }

      //Total words in this instance's document, not including itself
      val totalWordsInDocW: Double = sum(corpus.getDocTopicRow(word.doc)) - 1.0

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
          corpus.incrementTopicWord(word.topic, corpus.vocabulary(word.token))
          corpus.incrementDocTopic(word.doc, word.topic)

          //decrement counts of old topic assignment that has been changed
          corpus.decrementTopicWord(oldTopic, corpus.vocabulary(word.token))
          corpus.decrementDocTopic(word.doc, oldTopic)

        }
      }

    }

  }

  def getTheta {

    //we turn the counts matrix into a probability matrix
    for (doc <- 0 to corpus.docTopicMatrix.rows - 1) {

      //corpus.docTopicMatrix(doc, ::) := (corpus.docTopicMatrix(doc, ::) + alpha) / (sum(corpus.docTopicMatrix(doc, ::).t) + K * alpha)
      val countToProb:DenseVector[Double]=((corpus.docTopicMatrix(doc, ::) + alpha) / (sum(corpus.docTopicMatrix(doc, ::).t) + K * alpha)).t
      corpus.setDocTopicRow(doc,countToProb)

    }

  }

  def getPhi {

    //we turn the counts matrix into a probability matrix
    for (topic <- 0 to corpus.topicWordMatrix.rows - 1) {

      //corpus.topicWordMatrix(topic, ::) := (corpus.topicWordMatrix(topic, ::) + beta) / (sum(corpus.topicWordMatrix(topic, ::).t) + corpus.topicWordMatrix.cols * beta)
      val countToProb:DenseVector[Double]=((corpus.topicWordMatrix(topic, ::) + beta) / (sum(corpus.topicWordMatrix(topic, ::).t) + corpus.topicWordMatrix.cols * beta)).t
      corpus.setTopicWordRow(topic,countToProb)

    }

  }

  def printTopics(numWords: Int) {

    //want to actually show the words, so we need to extract strings from ids
    val reverseVocab = corpus.reverseVocab

    for (topic <- 0 to corpus.topicWordMatrix.rows - 1) {

      //tie probability to column index, then sort by probabiltiy, take the top numWords, map column index to corresponding word
      println("Topic #" + topic + ":  " + corpus.topicWordMatrix(topic, ::).t.toArray.zipWithIndex.sortBy(-_._1).take(numWords).toList.map(x => reverseVocab(x._2)))

    }
  }

  def printTopicProps(docIndex: Int, probCutoff: Double) {

    //tie probability to column index, filter probabilities by probCutoff
    println(corpus.docTopicMatrix(docIndex, ::).t.toArray.zipWithIndex.filter(x => x._1 > probCutoff).toList)

  }


}


