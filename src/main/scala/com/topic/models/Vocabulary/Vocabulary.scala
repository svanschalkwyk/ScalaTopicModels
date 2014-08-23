package com.topic.models.Vocabulary

import scala.collection.immutable.HashMap

trait Vocabulary {

  def getVocabulary: HashMap[String, Int]
}
