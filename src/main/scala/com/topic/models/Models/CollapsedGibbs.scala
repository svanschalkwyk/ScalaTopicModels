package com.topic.models.Models

import breeze.stats.distributions.Multinomial
import breeze.linalg.{Axis, sum, DenseVector}
import com.topic.models.Corpus.CollapsedLDACorpus
import com.topic.models.Word.Word


/**
 * Collapsed Gibbs sampling inference algorithmm for Latent Dirichlet Allocation.
 */
class CollapsedGibbs(corpus: CollapsedLDACorpus, docDirectory: String, vocabThreshold: Int, K: Int, alpha: Double, beta: Double) extends TopicModel {


  /**
   * For a given word, calculate the conditional distribution over topic assignments to be sampled from.
   * @param word word whose topic will be inferred from the Gibb's sampler.
   * @return distribution over topics for the word input.
   */
  private[this] def gibbsDistribution(word: Word): Multinomial[DenseVector[Double], Int] = {

    val docTopicRow: DenseVector[Double] = corpus.docTopicMatrix(word.doc, ::).t

    val topicWordCol: DenseVector[Double] = corpus.topicWordMatrix(::, corpus.vocabulary(word.token))

    val topicSums: DenseVector[Double] = sum(corpus.topicWordMatrix, Axis._1)

    val params = (docTopicRow + alpha) :* (topicWordCol + beta) / (topicSums + corpus.vocabulary.size * beta)

    //normalize parameters
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

      println(iter)

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

  /**
   * Calculate theta matrix directly from doc/topic counts.  Overwrite counts matrix to save memory.
   */
  private[this] def getTheta {

    //we turn the counts matrix into a probability matrix
    for (doc <- 0 to corpus.numDocs - 1) {

      val countToProb: DenseVector[Double] = ((corpus.docTopicMatrix(doc, ::) + alpha) / (sum(corpus.docTopicMatrix(doc, ::).t) + K * alpha)).t
      corpus.docTopicMatrix(doc, ::) := countToProb.t

    }
  }


  /**
   * Calculate phi matric directly from topic/word counts.  Overwrite counts matrix to save memory.
   */
  private[this] def getPhi {

    //we turn the counts matrix into a probability matrix
    for (topic <- 0 to K - 1) {

      val countToProb: DenseVector[Double] = ((corpus.topicWordMatrix(topic, ::) + beta) / (sum(corpus.topicWordMatrix(topic, ::).t) + corpus.vocabulary.size * beta)).t
      corpus.topicWordMatrix(topic, ::) := countToProb.t

    }
  }

  /**
   * Perform all inference steps - gibbs sampling, calculating theta matrix, calculating phi matrix.
   */
  def inference {

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
    val revV = corpus.reverseVocab

    for (topic <- 0 to K - 1) {

      //tie probability to column index, then sort by probabiltiy, take the top numWords, map column index to corresponding word
      println("Topic #" + topic + ":  " + corpus.topicWordMatrix(topic, ::).t.toArray.zipWithIndex.sortBy(-_._1).take(numWords).toList.map(x => revV(x._2)))

    }
  }

  /**
   * For a given document, display most likely topics occurring in it.
   * @param docIndex index of document to be analyzed.
   * @param probCutoff Determines how likely a topic has to be for it to be displayed.
   */
  def printTopicProps(docIndex: Int, probCutoff: Double) {

    //tie probability to column index, filter probabilities by probCutoff
    println(corpus.docTopicMatrix(docIndex, ::).t.toArray.zipWithIndex.filter(x => x._1 > probCutoff).toList)

  }

}


