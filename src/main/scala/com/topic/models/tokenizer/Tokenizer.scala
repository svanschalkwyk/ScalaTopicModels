package com.topic.models.tokenizer

import java.io.File

/**
 * Tokenizer trait that should be extended by tokenizer to tokenize both files and strings.
 */
trait Tokenizer {

  def tokenizeFile(x: File): List[String]

  def tokenizeString(x: String): List[String]
}
