package com.topic.models.corpus

import java.io.File
import com.topic.models.tokenizer.StanfordTokenizer
import com.topic.models.vocabulary.CountVocab
import scala.collection.immutable.HashMap


class StreamingCorpus(batchSize: Int, docsDirectory: String, minCount: Int) extends StanfordTokenizer with Corpus {

  var vocabulary = CountVocab(docsDirectory, minCount).getVocabulary
  var batchFileList: List[List[File]] = List.empty
  var curIndx = 0

  def docsSeen() = batchSize * curIndx

  def checkIfDone(): Boolean = if (curIndx < batchFileList.size - 1) true else false

  /**
   * Get the list of files to stream through and split them into minibatches.
   */
  def initialize = {
    //List all of the files in the document directory
    val fileList = new File(docsDirectory).listFiles.toList

    //Split the files into minibatches
    batchFileList = fileList.grouped(batchSize).toList
  }

  /**
   * Get a bag-of-words representation of each document in the minibatch.
   * @return list of documents in minibatch in bag-of-words format.
   */
  def getNextMiniBatch: List[List[(Int, Int)]] = {

    var miniBatch: List[List[(Int, Int)]] = List.empty

    val curMiniBatchFiles = batchFileList(curIndx)

    for (batchFile <- curMiniBatchFiles) {
      miniBatch = miniBatch ++ List(docBOW(batchFile))
    }

    curIndx += 1

    miniBatch
  }

  /**
   * Put document in bag-of-words format.
   * @param docFile Document file.
   * @return Document in bag-of-words format.
   */
  def docBOW(docFile: File): List[(Int, Int)] = {

    var BOW: HashMap[Int, Int] = HashMap.empty

    val tokenizer = new StanfordTokenizer
    val tokens = tokenizer.tokenizeFile(docFile)

    for (token <- tokens) {
      if (vocabulary.contains(token)) {

        if (BOW.contains(vocabulary(token))) BOW += (vocabulary(token) -> (BOW(token) + 1))
        else BOW += (vocabulary(token) -> 1)

      }
    }
    BOW.toList
  }
}
