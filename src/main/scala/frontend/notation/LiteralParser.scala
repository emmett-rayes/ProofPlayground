package proofPlayground
package frontend.notation

import frontend.notation.Tokens
import frontend.notation.parser.{ParseError, Parser}

import scala.util.Try

type Literal[T] = Singleton & T

object LiteralParser:
  /** Creates a parser that matches the given literal string.
    *
    * @tparam S the literal string to match.
    * @return a parser that produces the matched literal string.
    */
  def parser[S <: Literal[String]](using expected: ValueOf[S]): Parser[Tokens, S] =
    input =>
      val trimmed = input.skipWhitespace
      Try {
        if !trimmed.startsWith(expected.value.asTokens) then
          throw ParseError(input, s"Expected ${expected.value} at this position $trimmed.")
        trimmed.splitAt(expected.value.length) match
          case (matched, remaining) => (remaining, matched.mkString.asInstanceOf[S])
      }
