package proofPlayground
package parser

/** A token is a single character in the input string to be parsed. */
type Token = Char

/** A sequence of tokens representing the input string to be parsed. */
type Tokens = IndexedSeq[Token]

extension (self: String) {

  /** Converts the string into a sequence of tokens.
    *
    * @return the sequence of tokens representing the string.
    */
  def asTokens: Tokens = self
}

extension (tokens: Tokens) {

  /** Skips leading whitespace tokens and returns the remaining tokens.
    *
    * @return the remaining tokens after skipping leading whitespace.
    */
  def skipWhitespace: Tokens = tokens.span(_.isWhitespace)._2
}
