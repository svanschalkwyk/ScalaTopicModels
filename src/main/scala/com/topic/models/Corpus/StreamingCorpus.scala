package com.topic.models.Corpus

import java.io.File
import com.topic.models.Tokenizer.StanfordTokenizer
import com.topic.models.Vocabulary.CountVocab
import scala.collection.immutable.HashMap


class StreamingCorpus(docsDirectory:String,batchSize:Int, minCount:Int) extends StanfordTokenizer with Corpus{

  val vocabulary=CountVocab(docsDirectory,minCount).getVocabulary
  var batchFileList:List[List[File]]=List.empty
  var curIndx=0

  /**
   * Get the list of files to stream through and split them into minibatches.
   */
  def initialize={
    //List all of the files in the document directory
    val fileList=new File(docsDirectory).listFiles.toList

    //Split the files into minibatches
    batchFileList=fileList.grouped(batchSize).toList
  }

  /**
   * Get a bag-of-words representation of each document in the minibatch.
   * @return list of documents in minibatch in bag-of-words format.
   */
  def getNextMiniBatch:List[HashMap[String,Int]]={

    var miniBatch:List[HashMap[String,Int]]=List.empty

    val curMiniBatchFiles=batchFileList(curIndx)

    for(batchFile <- curMiniBatchFiles){
      miniBatch=miniBatch++List(docBOW(batchFile))
    }

    curIndx+=1

    miniBatch
  }

  /**
   * Put document in bag-of-words format.
   * @param docFile Document file.
   * @return Document in bag-of-words format.
   */
  def docBOW(docFile:File):HashMap[String,Int]={

    var BOW:HashMap[String,Int]=HashMap.empty

    val tokenizer = new StanfordTokenizer
    val tokens = tokenizer.tokenizeFile(docFile)

    while(tokens.hasNext){
      val token=tokens.next().word().toLowerCase()

      if(vocabulary.contains(token)){

        if(BOW.contains(token)) BOW += (token -> (BOW(token)+1))
        else BOW +=(token -> 1)

      }
    }
    BOW
  }
}
