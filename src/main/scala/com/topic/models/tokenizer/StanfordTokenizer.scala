package com.topic.models.tokenizer

import edu.stanford.nlp.process.{CoreLabelTokenFactory, PTBTokenizer}
import java.io.{StringReader, FileReader}
import scala.collection.JavaConversions._

/**
 * Tokenizer as implemented by the Stanford tokenizer.  The tokenizer returns a list of tokens.
 */
class StanfordTokenizer extends Tokenizer {

  def tokenizeFile(docFile: java.io.File) = new PTBTokenizer(new FileReader(docFile), new CoreLabelTokenFactory(), "").tokenize.map(x => x.word().toLowerCase).toList

  def tokenizeString(str: String) = new PTBTokenizer(new StringReader(str), new CoreLabelTokenFactory(), "").tokenize.map(x => x.word.toLowerCase()).toList

}
