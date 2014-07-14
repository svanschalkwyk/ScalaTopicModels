package org.scalatopicmodels

/**
 * Created by alex on 12/07/14.
 */
class collapsedGibbs(docDirectory:String,vocabThreshold:Int,K:Int,alpha:Double,beta:Double) {

  //create corpus instance
  val corpus=new Corpus(docDirectory)

  //if vocabulary is not provided, create one from the documents themselves.

  corpus.getVocabulary(vocabThreshold)


  corpus.initialize(K)


  def gibbsDistribution(word:Word){
    println(word.token)

    var multinomialParams:List[Double]=List.empty

    var wAssignedToTopic=0
    var wordsInDocWAssignedtoTopic=0

    //Iterate over topics
    for(topic<- 1 to K){

      if(corpus.wordTopicCounts.contains((word.token,topic))){
        wAssignedToTopic=corpus.wordTopicCounts((word.token,topic)) - 1
      }

      //var wAssignedToTopic=corpus.wordTopicCounts((word.token,topic)) - 1
      //var totalWAssignedtoTopic=corpus.wordTopicCounts.keys.filter(x => x._2==topic).size -1



      var totalAssignedtoTopic=corpus.words.filter(x => x.topic == topic).size -1



      if(corpus.docTopicCounts.contains((word.doc,topic))){
        wordsInDocWAssignedtoTopic=corpus.docTopicCounts((word.doc,topic)) -1
      }
      //var totalWordsInDocW=corpus.words.filter(x => x.doc==word.doc).size -1  //This could be pre-computed at initialization



      var totalWordsInDocW=corpus.docSize(word.doc)

      var paramK=((wAssignedToTopic+beta)/(totalAssignedtoTopic+corpus.vocabulary.size*beta))*(wordsInDocWAssignedtoTopic+alpha)/(totalWordsInDocW+K*alpha)

      print(paramK)





    }
    println()

  }

  def gibbsSample{

    for(word <- corpus.words){
      gibbsDistribution(word)
    }

  }






}
