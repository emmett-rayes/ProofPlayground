package proofPlayground
package frontend.notation.parser

import frontend.notation.parser.{ParseError, Parser}

import scala.util.Try

object LiteralParser:
  /** Creates a parser that matches the given literal string.
    *
    * @tparam S the literal string to match.
    * @return a parser that produces the matched literal string.
    */
  def parser[S <: String: Singleton](using expected: ValueOf[S]): Parser[Tokens, S] =
    input =>
      val trimmed = input.skipWhitespace
      Try {
        if !trimmed.startsWith(expected.value.asTokens) then
          throw ParseError(input, s"Expected ${expected.value} at this position $trimmed.")
        trimmed.splitAt(expected.value.length) match
          case (matched, remaining) => (remaining, matched.mkString.asInstanceOf[S])
      }
