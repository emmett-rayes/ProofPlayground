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
          case None    => throw ParseError(input, s"Expected input that matches ${regex.regex}")
          case Some(m) => (input.mkString.substring(m.end), input.mkString.substring(m.start, m.end))
      }
