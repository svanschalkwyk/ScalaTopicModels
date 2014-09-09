package com.topic.models.vocabulary

import scala.collection.immutable.HashMap

trait Vocabulary {

  def getVocabulary: HashMap[String, Int]
}
