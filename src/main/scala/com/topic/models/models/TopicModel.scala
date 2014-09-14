package com.topic.models.models

/**
 * Created by alex on 23/08/14.
 */
trait TopicModel {

  def inference: Unit

  def printTopics(numWords:Int): Unit
}
