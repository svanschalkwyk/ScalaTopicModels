package com.topic.models.utils

import com.topic.models.tokenizer.StanfordTokenizer
import scala.collection.immutable.HashMap
import java.io.File

object DocUtils {

  def numDocs(docDirectory:String):Int=new File(docDirectory).listFiles.toIterator.filter(_.getName.endsWith(".txt")).size


  def docSize(doc: String): Int = {

    val tokens = StanfordTokenizer.tokenizeString(doc)
    tokens.size
  }

  def getBOW(doc: String, vocab: HashMap[String, Int]): List[(Int, Int)] = {

    var BOW: HashMap[Int, Int] = HashMap.empty

    val tokens = StanfordTokenizer.tokenizeString(doc)

    for (token <- tokens) {
      if (vocab.contains(token)) {

        if (BOW.contains(vocab(token))) BOW += (vocab(token) -> (BOW(vocab(token)) + 1))
        else BOW += (vocab(token) -> 1)

      }
    }
    BOW.toList
  }
}



