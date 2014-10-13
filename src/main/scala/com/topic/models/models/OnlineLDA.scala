package com.topic.models.models

import breeze.linalg._
import breeze.numerics._
import breeze.stats.distributions.Gamma
import breeze.stats.mean

import scala.collection.immutable.HashMap
import com.topic.models.corpus.StreamingCorpus


class OnlineLDA(corpus: StreamingCorpus, numTopics: Int, decay: Double, docNum: Int, withPerplexity: Boolean = false) extends TopicModel {

  //initialise parameters
  val vocabulary = corpus.vocabulary
  val miniBatchSize=corpus.batchSize
  var postsSeen = 0
  var numUpdates = 0
  var rho = 0.0
  val numTerms = vocabulary.size
  val eta = 1.0 / numTopics
  val alpha = 1.0 / numTopics
  val gammaThreshold = 0.001
  val iterations = 100
  //global parameter
  var sstats = new DenseMatrix[Double](numTopics, numTerms, Gamma(100.0, 1.0 / 100.0).sample(numTopics * numTerms).toArray)
  val idToWord = vocabulary.map(_.swap)

  def getLambda = sstats + eta


  def eStep(miniBatch: List[List[(Int, Int)]], expELogBeta: DenseMatrix[Double]): (DenseMatrix[Double], DenseMatrix[Double]) = {

    //If testing, want to keep this non-random
    val gamma = new DenseMatrix[Double](miniBatch.length, numTopics, Gamma(100.0, 1.0 / 100.0).sample(numTopics * miniBatch.length).toArray)

    val eLogTheta = dirichletExpectation(gamma)
    val expELogTheta = exp(eLogTheta)
    var sstats = DenseMatrix.zeros[Double](numTopics, numTerms)

    for ((doc, idx) <- miniBatch.zipWithIndex) {

      val idCtList = doc.toList.sortBy(_._1)
      val wordIDs = idCtList.map(_._1)
      val cts = idCtList.map(_._2.toDouble)

      val gammaD: DenseMatrix[Double] = gamma(idx, ::).t.toDenseMatrix

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
        exp(dirichletExpectation(gD))
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

    (gamma, sstats)
  }


  /**
   * Computes expected value of log of vector (or matrix) of Dirichlet random variables.
   * @param hParam DenseMatrix of Dirichlet random variable.
   * @return Expected value of log of hParam.
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


  /**
   * Function that merges the sstat parameter of the current minibatch with the global sstat parameter.
   * @param rhoT  parameter weighting the influence of the current minibatch's sstat parameter.
   * @param sstat sstat parameter of current minibatch.
   * @return Quantity to add to the global sstat parameter.
   */
  def blend(rhoT: Double, sstat: DenseMatrix[Double]): DenseMatrix[Double] = {

    val scale = postsSeen.toDouble / miniBatchSize

    sstat :* (rhoT * scale)
  }


  /**
   * Print the topics learned by the LDA model.
   * @param numWords Number of words to display for each topic.
   */
  def printTopics(numWords: Int) {

    var sortMap = HashMap[String, Double]()
    val curLambda: DenseMatrix[Double] = getLambda

    for (topic <- 0 to numTopics - 1) {

      val topicProbs = curLambda(topic, ::) / sum(curLambda(topic, ::).t)

      //Want to sort while keeping track of indexes.  Use a HashMap that is later sorted by value.
      for ((prob, wordId) <- topicProbs.t.toArray.zipWithIndex) {
        sortMap += (idToWord(wordId) -> prob)
      }
      //Now sort by probability, take first n words and print them.
      println("Topic #" + topic + ": " + sortMap.toList.sortBy(-_._2).take(numWords))
    }
  }


  /**
   * Compute the perplexity associated with the current minibatch.
   * @return Tuple containing perplexity score and total number of words in the minibatch.
   */
  def perplexity(bow: List[List[(Int, Int)]], gamma: DenseMatrix[Double]): Double = {

    var score = 0.0

    val eLogTheta = dirichletExpectation(gamma)

    val eLogBeta = dirichletExpectation(getLambda)

    for ((doc, idx) <- bow.zipWithIndex) {

      val wordIDs = doc.map(x => x._1)
      val wordCts = doc.map(x => x._2.toDouble)

      val phiNorm = DenseVector.zeros[Double](wordIDs.length)

      for ((id, id_idx) <- wordIDs.zipWithIndex) {

        var temp = eLogTheta + eLogBeta(::, id).toDenseMatrix

        var tmax = max(temp)

        phiNorm(id_idx) = breeze.numerics.log(sum(exp(temp - tmax))) + tmax
      }

      val docCounts = DenseVector(wordCts.toArray)

      score += sum(docCounts :* phiNorm)
    }

    score += sum(((-gamma) + alpha) :* eLogTheta)

    score += sum(lgamma(gamma) - lgamma(alpha))

    score += sum(-lgamma(sum(gamma, Axis._1)) + lgamma(alpha * numTopics))

    score = score * (docNum / miniBatchSize)

    score += sum((-getLambda + eta) :* eLogBeta)

    score += sum(lgamma(getLambda) - lgamma(eta))

    score += sum(-lgamma(sum(getLambda, Axis._1)) + lgamma(eta * vocabulary.size))

    score
  }

  def inference() = {

    while (corpus.checkIfDone()) {
      val mbBOW = corpus.getNextMiniBatch

      //Get global parameter which gets updated after each mini-batch
      val expELogBeta = exp(dirichletExpectation(getLambda))

      //perfrom E-Step of LDA algorithm in parallel using minibatch and global parameter as input
      val sstatsGammaTuple = eStep(mbBOW, expELogBeta)
      val gammaLocal = sstatsGammaTuple._1
      val sstatsLocal = sstatsGammaTuple._2

      //Get perplexity of current minibatch. Only necessary for model comparison. Slows things down considerably!
      if (withPerplexity) miniBatchPerplexity(mbBOW, gammaLocal)

      //Blend local update from minibatch with global parameter
      //println(sstats.cols,sstats.rows)
      //val blendParam=blend(rho,sstatsLocal)
      //println(blendParam.cols,blendParam.cols)
      sstats = sstats + blend(rho,sstatsLocal)

      //update other LDA parameters
      postsSeen += miniBatchSize
      numUpdates += 1
      rho = pow(1.0 + numUpdates, -decay)
    }
  }

  def miniBatchPerplexity(bow: List[List[(Int, Int)]], gamma: DenseMatrix[Double]) {

    //compute perplexity for this minibatch
    val score = perplexity(bow, gamma)

    val totalWords: Double = sum(bow.map(x => sum(x.map(y => y._2))))

    println("Minibatch perplexity: " + exp(-score * miniBatchSize / (docNum * totalWords)))
  }
}

