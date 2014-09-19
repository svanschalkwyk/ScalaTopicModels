package com.topic.models.corpus

import scala.collection.immutable.HashMap
import scala.io.Source
import com.topic.models.tokenizer.StanfordTokenizer
import java.io.File

class StreamingCorpus(vocab: HashMap[String, Int], batchSize: Int, docsDirectory: String) extends StanfordTokenizer with Corpus {

  var vocabulary = vocab
  var batchFileList: List[List[File]] = List.empty
  var curIndx = 0

  def docsSeen() = batchSize * curIndx

  def checkIfDone(): Boolean = if (curIndx < batchFileList.size - 1) true else false


  def initialize = {
    val fileList = new File(docsDirectory).listFiles.toIterator.filter(_.getName.endsWith(".txt")).toList
    batchFileList = fileList.grouped(batchSize).toList
  }


  def getNextMiniBatch: List[List[(Int, Int)]] = {

    var miniBatch: Vector[List[(Int, Int)]] = Vector.empty

    val curMiniBatchFiles = batchFileList(curIndx)

    for (batchFile <- curMiniBatchFiles) {
      miniBatch :+= docBOW(Source.fromFile(batchFile).getLines().mkString)
    }

    curIndx += 1

    miniBatch.toList
  }


  def docBOW(docFile: String): List[(Int, Int)] = {

    var BOW: HashMap[Int, Int] = HashMap.empty

    val tokenizer = new StanfordTokenizer
    val tokens = tokenizer.tokenizeString(Source.fromString(docFile).getLines.mkString)

    for (token <- tokens) {
      if (vocabulary.contains(token)) {

        if (BOW.contains(vocabulary(token))) BOW += (vocabulary(token) -> (BOW(vocabulary(token)) + 1))
        else BOW += (vocabulary(token) -> 1)

      }
    }
    BOW.toList
  }
}
