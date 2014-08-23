package com.topic.models.Models.CollapsedGibbsLDA

import breeze.stats.distributions.Multinomial
import breeze.linalg.{sum, DenseVector}


/**
 * Collapsed Gibbs sampling inference algorithmm for Latent Dirichlet Allocation.
 */
class collapsedGibbs(docDirectory: String, vocabThreshold: Int, K: Int, alpha: Double, beta: Double) {

  //create corpus instance
  val corpus = new Corpus(docDirectory, vocabThreshold)

  //Randomly initialize topic assignments
  corpus.initialize(K)

  /**
   * For a given word, calculate the conditional distribution over topic assignments to be sampled from.
   * @param word word whose topic will be inferred from the Gibb's sampler.
   * @return distribution over topics for the word input.
   */
  private[this] def gibbsDistribution(word: Word): Multinomial[DenseVector[Double], Int] = {

    var multinomialParams: List[Double] = List.empty

    var wAssignedToTopic = 0.0
    var wordsInDocWAssignedtoTopic = 0.0

    /*

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
      //val paramK = ((wAssignedToTopic + beta) / (totalAssignedtoTopic + corpus.vocabulary.size * beta)) * (wordsInDocWAssignedtoTopic + alpha) / (totalWordsInDocW + K * alpha)
      val paramK =  (wordsInDocWAssignedtoTopic+alpha)*(wAssignedToTopic+beta)/(totalAssignedtoTopic+corpus.vocabulary.size * beta)

      multinomialParams = multinomialParams ++ List(paramK)
    }
    */
    val docTopicRow:DenseVector[Double]=corpus.getDocTopicRow(word.doc)

    val topicWordCol:DenseVector[Double]=corpus.getTopicWordCol(corpus.vocabulary(word.token))

    val topicSums:DenseVector[Double]=corpus.getTopicSums

    val params=(docTopicRow+alpha):*(topicWordCol+beta)/(topicSums+corpus.vocabulary.size * beta)

    //normalize parameters
    //val unnormalized = DenseVector(multinomialParams.toArray)
    //val normalizingConstant = sum(unnormalized)
    val normalizingConstant = sum(params)
    val normalizedParams = params :/ normalizingConstant

    Multinomial(normalizedParams)
  }

  /**
   * Gibbs sampler for LDA
   * @param numIter number of iterations that Gibbs sampler will be run
   */
  private[this] def gibbsSample(numIter: Int = 200) {

    for (iter <- 0 to numIter) {

      //if(iter%10==0){
        println(iter)
      //}

      for (word <- corpus.getWords) {
        //println(word.token)
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

  /**
   * Calculate theta matrix directly from doc/topic counts.  Overwrite counts matrix to save memory.
   */
  private[this] def getTheta {

    //we turn the counts matrix into a probability matrix
    for (doc <- 0 to corpus.getNumDocs - 1) {

      val countToProb:DenseVector[Double]=((corpus.getDocTopicRow(doc).t + alpha) / (sum(corpus.getDocTopicRow(doc)) + K * alpha)).t
      corpus.setDocTopicRow(doc,countToProb)

    }

  }


  /**
   * Calculate phi matric directly from topic/word counts.  Overwrite counts matrix to save memory.
   */
  private[this] def getPhi {

    //we turn the counts matrix into a probability matrix
    for (topic <- 0 to K - 1) {

      val countToProb:DenseVector[Double]=((corpus.getTopicWordRow(topic).t + beta) / (sum(corpus.getTopicWordRow(topic)) + corpus.vocabulary.size * beta)).t
      corpus.setTopicWordRow(topic,countToProb)

    }

  }

  /**
   * Perform all inference steps - gibbs sampling, calculating theta matrix, calculating phi matrix.
   */
  def inference{

    gibbsSample()

    getTheta

    getPhi
  }

  /**
   * Print topics found by LDA algorithm.
   * @param numWords Determines how many words to display for each topic.
   */
  def printTopics(numWords: Int) {

    //want to actually show the words, so we need to extract strings from ids
    val reverseVocab = corpus.reverseVocab

    for (topic <- 0 to K - 1) {

      //tie probability to column index, then sort by probabiltiy, take the top numWords, map column index to corresponding word
      println("Topic #" + topic + ":  " + corpus.getTopicWordRow(topic).toArray.zipWithIndex.sortBy(-_._1).take(numWords).toList.map(x => reverseVocab(x._2)))

    }
  }

  /**
   * For a given document, display most likely topics occurring in it.
   * @param docIndex index of document to be analyzed.
   * @param probCutoff Determines how likely a topic has to be for it to be displayed.
   */
  def printTopicProps(docIndex: Int, probCutoff: Double) {

    //tie probability to column index, filter probabilities by probCutoff
    println(corpus.getDocTopicRow(docIndex).toArray.zipWithIndex.filter(x => x._1 > probCutoff).toList)

  }


}


