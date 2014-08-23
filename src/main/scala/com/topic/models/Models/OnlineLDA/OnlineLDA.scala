package com.topic.models.Models.OnlineLDA

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.Gamma
import breeze.stats.mean

import scala.collection.immutable.HashMap


/**
 * Created by aminnaar on 2014-07-16.
 */


object dirObject {

  /**
   * Computes expected value of log of vector (or matrix) of Dirichlet random variables.
   * @param hParam DenseMatrix of Dirichlet random variable
   * @return Expected value of log of hParam
   */
  def dirichletExpectation(hParam: DenseMatrix[Double]): DenseMatrix[Double] = {

    if (hParam.rows == 1 || hParam.cols == 1) {

      val innerDigamma = digamma(sum(hParam))
      return digamma(hParam) - innerDigamma
    }
    //Note: Breeze will not allow two applies on the same line so this calculation has to be done in two lines instead of one.
    val first_term = digamma(hParam)
    first_term(::, *) - digamma(sum(hParam, Axis._1))
  }

}

/**
 * case class that holds and updates sstats matrix.
 * @param eta Global parameter for LDA model.
 * @param topics number of topics which determines sstats matrix shape.
 * @param terms number of distinct words in vocabulary which determines sstats matrix shape.
 */
case class LdaState(eta: Double, topics: Int, terms: Int) {

  var sstats = DenseMatrix.zeros[Double](topics, terms)
  var numDocs = 0

  /**
   * Gets lambda parameters given sstats and eta.
   * @return lambda parameters as DenseMatrix.
   */
  def getLambda: DenseMatrix[Double] = {
    sstats + eta
  }

  /**
   * Gets expected value of log of lambda.
   * @return expected value of log of lambda parameters.
   */
  def getELogBeta: DenseMatrix[Double] = {
    dirObject.dirichletExpectation(getLambda)
  }

  /**
   * Blends sstats from all seen documents with sstats from new mini-batch.
   * @param rhoT parameter affecting the influence of new mini-batch.
   * @param other LdaState object containing sstats and numDocs of new mini-batch.
   */
  def blend(rhoT: Double, other: LdaState) {

    var scale = 1.0

    sstats = sstats :* (1.0 - rhoT)

    if (other.numDocs == 0 || numDocs == other.numDocs) {
      scale = 1.0
    }
    else {
      scale = 1.0 * (numDocs.toDouble / other.numDocs)
    }

    sstats = sstats + (other.sstats :* (rhoT * scale))

  }

}

/**
 * class implementing the online LDA topic modeling algorithm.
 * @param corpus List of documents in bag-of-words format.
 * @param numTopics Number of topics for LDA model.
 * @param id2Word Dictionary mapping integer ids to vocabulary words.
 * @param chunkSize Size of mini-batches seen at each step.
 * @param test Set to true if testing, eliminates random intialization.
 */

class OnlineLDA(corpus: List[HashMap[Int, Int]], numTopics: Int, id2Word: HashMap[Int, String], chunkSize: Int, test: Boolean = false) {

  val evalEvery = 10
  val iterations = 50
  val decay = 0.5
  val gammaThreshold = 0.001
  val numTerms = id2Word.size
  val alpha = 1.0 / numTopics
  val eta = 1.0 / numTopics
  var expELogBeta: DenseMatrix[Double] = _
  var numUpdates = 0

  val state = LdaState(eta, numTopics, numTerms)

  //If testing, want to keep this non-random
  state.sstats = test match {
    case true => DenseMatrix.zeros[Double](numTopics, numTerms)
    case _ => new DenseMatrix[Double](numTopics, numTerms, Gamma(100.0, 1.0 / 100.0).sample(numTopics * numTerms).toArray)
  }

  syncState

  update(corpus)

  /**
   * Get expELogBeta of current state.
   */
  def syncState {
    expELogBeta = exp(state.getELogBeta)
  }

  /**
   * Perform E-Step on new mini-batch and use the result to update sstats of current model in M-Step.
   * @param corpus New mini-batch that will be used to update model.
   */
  def update(corpus: List[HashMap[Int, Int]]) {

    state.numDocs += corpus.length

    var realLen = 0

    var other = LdaState(eta, state.sstats.rows, state.sstats.cols)

    for (chunk <- corpus.grouped(chunkSize)) {
      val rho = pow(1.0 + numUpdates, -decay)

      realLen += chunk.length

      eStep(chunk, other)

      //gensim then does 'del chunk'.  Don't think there is a Scala equivalent for this.

      mStep(rho, other)

      //gensim then does 'del other'.  Don't think there is a Scala equivalent for this.  We are over-writing it in the next line anyways...

      other = LdaState(eta, state.sstats.rows, state.sstats.cols)

    }

  }

  /**
   * Infer gamma and sstats parameters for current chunk of mini-batch.
   * @param chunk Chunk of mini-batch of new documents.
   * @return gamma and sstats parameters.
   */
  private[this] def inference(chunk: List[HashMap[Int, Int]]): (DenseMatrix[Double], DenseMatrix[Double]) = {

    //If testing, want to keep this non-random
    val gamma = test match {
      case true => DenseMatrix.zeros[Double](chunk.length, numTopics) + 1.0
      case _ => new DenseMatrix[Double](chunk.length, numTopics, Gamma(100.0, 1.0 / 100.0).sample(numTopics * chunk.length).toArray)
    }

    val eLogTheta = dirObject.dirichletExpectation(gamma)
    val expELogTheta = exp(eLogTheta)
    var sstats = DenseMatrix.zeros[Double](numTopics, numTerms)

    for ((doc, idx) <- chunk.zipWithIndex) {

      //Gensim sorts list by wordID, I'm not sure if this is necessary.
      val idCtList = doc.toList.sortBy(_._1)
      val wordIDs = idCtList.map(_._1)
      val cts = idCtList.map(_._2.toDouble)

      val gammaD: DenseMatrix[Double] = gamma(idx, ::).t.toDenseMatrix

      var eLogThetaD = eLogTheta(idx, ::).t.toDenseMatrix

      val expELogThetaD: DenseMatrix[Double] = expELogTheta(idx, ::).t.toDenseMatrix

      val expELogBetaD = expELogBeta(0 until expELogBeta.rows, wordIDs.toIndexedSeq).toDenseMatrix

      val phiNorm: DenseMatrix[Double] = expELogThetaD * expELogBetaD + 1e-100

      val docCounts = DenseMatrix(cts.toArray)

      //Recursive loop to infer phiNorm, gammaD and exoElogThetaD parameters
      def gammaUpdate(pn: DenseMatrix[Double], expETD: DenseMatrix[Double]): DenseMatrix[Double] = {
        val term1 = expETD :* (docCounts / pn) * expELogBetaD.t
        term1(::, *) + alpha
      }

      def thetaUpdate(gD: DenseMatrix[Double]): DenseMatrix[Double] = {
        exp(dirObject.dirichletExpectation(gD))
      }

      def phiUpdate(expETD: DenseMatrix[Double]): DenseMatrix[Double] = {
        expETD * expELogBetaD + 1e-100
      }

      //Attempt at making a recursive function to replace for loop (because they are slow).
      def eStepIterator(phiNorm: DenseMatrix[Double], expELogThetaD: DenseMatrix[Double], gammaD: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]) = {

        val lastGamma = DenseMatrix.zeros[Double](gammaD.rows, gammaD.cols)

        def loop(counter: Int, newGamma: DenseMatrix[Double], newTheta: DenseMatrix[Double], newPhi: DenseMatrix[Double], lastGamma: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double], DenseMatrix[Double]) = {
          if (((mean(abs(newGamma - lastGamma))) < gammaThreshold) || (counter > iterations)) {
            (newGamma, newPhi, newTheta)
          }
          else {
            val term1 = gammaUpdate(newPhi, newTheta)
            val term2 = thetaUpdate(term1)
            val term3 = phiUpdate(term2)
            loop(counter + 1, term1, term2, term3, newGamma)
          }
        }
        loop(0, gammaD, expELogThetaD, phiNorm, lastGamma)
      }

      //execute recursive loop function
      val (newGammaD, newPhiNorm, newExpELogThetaD) = eStepIterator(phiNorm, expELogThetaD, gammaD)

      gamma(idx, ::) := newGammaD.toDenseVector.t

      val sstatTerm = newExpELogThetaD.t * (docCounts / newPhiNorm)

      for ((i, ct) <- wordIDs.zipWithIndex) {
        sstats(::, i) :+= sstatTerm(::, ct)
      }

    }

    sstats = sstats :* expELogBeta

    return (gamma, sstats)

  }

  /**
   * Perform E-Step on chunk of mini-batch.
   * @param chunk chunk of collection of documents in mini-batch.
   * @param other LdaState object of mini-batch. Later to be blended with overall LdaState object.
   */
  private[this] def eStep(chunk: List[HashMap[Int, Int]], other: LdaState) = {

    val chunkSstats = inference(chunk)._2

    other.sstats = other.sstats + chunkSstats

    other.numDocs = other.numDocs + chunk.size
  }

  /**
   * Perform M-Step where learned parameters from E-Step are used to blend mini-batch with overall model.
   * @param rho Blending parameter affecting level of influence of mini-batch on overall model.
   * @param other LdaState object for new mini-batch to be blended with overall LdaState object.
   */
  private[this] def mStep(rho: Double, other: LdaState) {

    state.blend(rho, other)

    syncState

    numUpdates += 1
  }

  /**
   * Print all of the topics learned.
   * @param numWords Determines how many words to show for each topic, sorted by likelihood.
   */
  def showTopics(numWords: Int) {

    var sortMap = HashMap[String, Double]()
    val curLambda = state.getLambda

    for (topic <- 0 to numTopics - 1) {

      val topicProbs = curLambda(topic, ::) / sum(curLambda(topic, ::).t)

      //Want to sort while keeping track of indexes.  Use a HashMap that is later sorted by value.
      for ((prob, wordId) <- topicProbs.t.toArray.zipWithIndex) {
        sortMap += (id2Word(wordId) -> prob)
      }
      //Now sort by probability, take first n words and print them.
      println("Topic #" + topic + ": " + sortMap.toList.sortBy(-_._2).take(numWords))

    }
  }

  /**
   * Method to get the topic proportions for a list of new documents in bow format.
   * @param doc List of new documents in bow format.
   * @param probThreshold Probability threshold value for displaying topic.
   */
  def topicProportions(doc: List[HashMap[Int, Int]], probThreshold: Double = 0.1) {

    val gamma = inference(doc)._1

    //normalize to probability distribution
    val topicDist: DenseMatrix[Double] = gamma / sum(gamma)


    for ((topicProb, topic) <- topicDist.toDenseVector.toArray.zipWithIndex) {
      if (topicProb > probThreshold) {
        println("Topic " + topic + ", probability " + topicProb)
      }
    }
  }

  /**
   * calculate perplexity for test set of documents given the learned model.
   * @param docs test set of documents.
   * @return perplexity value.
   */
  def perplexity(docs: List[HashMap[Int, Int]]): Double = {

    val totalWords = sum(docs.map(x => sum(x.values)))

    val perWordBound = bound(docs) / totalWords

    return perWordBound
  }

  /**
   * Calculates lower bound on perplexity for the current docs.  Used to assess model performance.
   * @param docs List of strings where each element is a document.
   * @return Lower bound on perplexity for docs.
   */
  private[this] def bound(docs: List[HashMap[Int, Int]], subsampleRatio: Double = 1.0): Double = {

    var score = 0.0

    val lambda = state.getLambda

    val eLogBeta = dirObject.dirichletExpectation(lambda)

    for (docBOW <- docs) {

      val idCtList = docBOW.toList.sortBy(_._1)
      val wordIDs = idCtList.map(_._1)
      val cts = idCtList.map(_._2.toDouble)

      val gammaD = inference(List(docBOW))._1

      val eLogThetaD = dirObject.dirichletExpectation(gammaD)

      for ((id, id_idx) <- wordIDs.zipWithIndex) {

        score += cts(id_idx) * (log(sum(exp(eLogThetaD + eLogBeta(::, id).toDenseMatrix))))

      }

      score += sum((-gammaD + alpha) :* eLogThetaD)
      score += sum(lgamma(gammaD) - lgamma(alpha))
      score += lgamma(alpha * numTopics) - lgamma(sum(gammaD))

    }

    score *= subsampleRatio

    score += sum((-lambda + eta) :* eLogBeta)

    score += sum(lgamma(lambda) - lgamma(eta))

    score += sum(-lgamma(sum(lambda, Axis._1)) + lgamma(eta * numTerms.toDouble))

    return score
  }

}

