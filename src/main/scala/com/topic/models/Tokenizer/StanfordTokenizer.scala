package com.topic.models.Tokenizer

import edu.stanford.nlp.process.{CoreLabelTokenFactory, PTBTokenizer}
import java.io.{StringReader, FileReader}
import edu.stanford.nlp.ling.CoreLabel

/**
 * Tokenizer as implemented by the Stanford tokenizer.  The tokenizer returns an iterable of CoreLabel objects.
 */
class StanfordTokenizer[T] extends Tokenizer {

  type T = PTBTokenizer[CoreLabel]

  def tokenizeFile(docFile: java.io.File): PTBTokenizer[CoreLabel] = new PTBTokenizer(new FileReader(docFile), new CoreLabelTokenFactory(), "")

  def tokenizeString(str: String): PTBTokenizer[CoreLabel] = new PTBTokenizer(new StringReader(str), new CoreLabelTokenFactory(), "")
}
