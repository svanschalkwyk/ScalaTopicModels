package com.topic.models.Tokenizer

import java.io.File

/**
 * Tokenizer trait that should be extended by tokenizer to tokenize both files and strings.
 */
trait Tokenizer {

  type T

  def tokenizeFile(x: File): T

  def tokenizeString(x: String): T
}
