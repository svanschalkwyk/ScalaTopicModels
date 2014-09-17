package com.topic.models.corpus

import com.topic.models.tokenizer.StanfordTokenizer
import scala.collection.immutable.HashMap
import scala.io.Source


class StreamingCorpus(vocab: HashMap[String, Int], batchSize: Int, docsList: List[String]) extends StanfordTokenizer with Corpus {

  var vocabulary = vocab
  var batchFileList: List[List[String]] = List.empty
  var curIndx = 0

  def docsSeen() = batchSize * curIndx

  def checkIfDone(): Boolean = if (curIndx < batchFileList.size - 1) true else false

  /**
   * Get the list of files to stream through and split them into minibatches.
   */
  def initialize = {

    //Split the files into minibatches
    batchFileList = docsList.grouped(batchSize).toList
  }

  /**
   * Get a bag-of-words representation of each document in the minibatch.
   * @return list of documents in minibatch in bag-of-words format.
   */
  def getNextMiniBatch: List[List[(Int, Int)]] = {

    var miniBatch: Vector[List[(Int, Int)]] = Vector.empty

    val curMiniBatchFiles = batchFileList(curIndx)

    for (batchFile <- curMiniBatchFiles) {
      miniBatch :+= docBOW(batchFile)
    }

    curIndx += 1

    miniBatch.toList
  }

  /**
   * Put document in bag-of-words format.
   * @param docFile Document file.
   * @return Document in bag-of-words format.
   */
  def docBOW(docFile: String): List[(Int, Int)] = {

    var BOW: HashMap[Int, Int] = HashMap.empty

    val tokenizer = new StanfordTokenizer
    val tokens = tokenizer.tokenizeString(Source.fromFile(docFile).getLines.mkString)

    for (token <- tokens) {
      if (vocabulary.contains(token)) {

        if (BOW.contains(vocabulary(token))) BOW += (vocabulary(token) -> (BOW(vocabulary(token)) + 1))
        else BOW += (vocabulary(token) -> 1)

      }
    }
    BOW.toList
  }
}
