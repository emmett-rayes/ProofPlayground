package proofPlayground
package frontend.notation

import frontend.notation.parser.{ParseError, Parser}

import scala.util.Try
import scala.util.matching.Regex

object RegexParser:
  def parser(regex: Regex): Parser[Tokens, String] =
    input =>
      val trimmed = input.skipWhitespace
      Try {
        if trimmed.isEmpty then
          throw ParseError(input, "Expected input at this position.")
        regex.findPrefixMatchOf(trimmed.mkString) match
          case None    => throw ParseError(input, s"Expected a matching for ${regex.regex} at this position $trimmed.")
          case Some(m) => (trimmed.mkString.substring(m.end), trimmed.mkString.substring(m.start, m.end))
      }
