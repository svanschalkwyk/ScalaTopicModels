package com.topic.models.corpus

import scala.collection.immutable.HashMap
import scala.io.Source
import java.io.File
import com.topic.models.utils.DocUtils

class StreamingCorpus(vocab: HashMap[String, Int], batchSize: Int, docsDirectory: String) extends Corpus {

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

      val contents = Source.fromFile(batchFile).getLines().mkString
      miniBatch :+= DocUtils.getBOW(contents, vocab)
    }

    curIndx += 1

    miniBatch.toList
  }
}
