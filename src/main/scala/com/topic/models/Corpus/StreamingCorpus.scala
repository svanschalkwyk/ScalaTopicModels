package com.topic.models.Corpus

import java.io.File
import com.topic.models.Tokenizer.StanfordTokenizer
import com.topic.models.Vocabulary.CountVocab
import scala.collection.immutable.HashMap


class StreamingCorpus extends StanfordTokenizer with Corpus {

  var vocabulary: HashMap[String, Int] = HashMap.empty
  var batchSize: Int = 0
  var docsDirectory: String = ""
  var batchFileList: List[List[File]] = List.empty
  var curIndx = 0

  def setParams(directory: String, batch: Int, minC: Int) = {
    vocabulary = CountVocab(directory, minC).getVocabulary
    batchSize = batch
    docsDirectory = directory
  }

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
  def getNextMiniBatch: List[HashMap[String, Int]] = {

    var miniBatch: List[HashMap[String, Int]] = List.empty

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
  def docBOW(docFile: File): HashMap[String, Int] = {

    var BOW: HashMap[String, Int] = HashMap.empty

    val tokenizer = new StanfordTokenizer
    val tokens = tokenizer.tokenizeFile(docFile)

    for (token <- tokens) {
      if (vocabulary.contains(token)) {

        if (BOW.contains(token)) BOW += (token -> (BOW(token) + 1))
        else BOW += (token -> 1)

      }
    }
    BOW
  }
}
