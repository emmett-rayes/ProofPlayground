package proofPlayground
package frontend.notation.parser

import scala.util.{Failure, Success, Try}

/** The result of a parser, containing either the remaining input and the parsed output,
  * or a parsing error. See [[ParseError]].
  *
  * @tparam Input the type of the input to be parsed.
  * @tparam Output the type of the output produced by the parser.
  */
type ParserResult[Input, Output] = Try[(Input, Output)]

extension [Input, Output](self: ParserResult[Input, Output])
  /** @return the remaining input after parsing, if successful. */
  def remaining: Try[Input] = self.map(_._1)

  /** @return the parsed output, if successful. */
  def result: Try[Output] = self.map(_._2)

/** A parser that takes an input of type `Input` and produces an output of type `Output`.
  *
  * The parser can be combined with other parsers using combinators such as `flatMap` and `orElse`.
  *
  * @tparam Input the type of the input to be parsed.
  * @tparam Output the type of the output produced by the parser.
  */
trait Parser[Input, +Output]:
  /** Parses the given input and returns a `ParserResult`.
    *
    * @param input the input to be parsed.
    * @return a `ParserResult` containing either the remaining input and the parsed output,
    *         or a parsing error.
    */
  def parse(input: Input): ParserResult[Input, Output]

  /** Chains this parser with another parser that depends on the output of this parser.
    *
    * @param f a function that takes the output of this parser and returns another parser.
    * @tparam Mapped the type of the output produced by the resulting parser.
    * @return a new parser that first applies this parser, then applies the parser returned by `f`.
    */
  final def flatMap[Mapped](f: Output => Parser[Input, Mapped]): Parser[Input, Mapped] =
    input =>
      parse(input).flatMap((remaining, output) =>
        Try {
          f(output).parse(remaining) match
            case Failure(exception) => throw ParseError(input, message = exception.getMessage, cause = exception)
            case Success(result)    => result
        }
      )

  /** Tries this parser, and if it fails, tries the other parser.
    *
    * @param other the other parser to try if this parser fails.
    * @tparam Else the type of the output produced by the other parser.
    * @return a new parser that produces either the output of this parser or the output of the other parser.
    */
  final def orElse[Else](other: => Parser[Input, Else]): Parser[Input, Either[Output, Else]] =
    input =>
      parse(input).map(r => (r._1, Left(r._2))).orElse(other.parse(input).map(r => (r._1, Right(r._2))))

object Parser:
  /** Creates a parser that always succeeds with the given output without consuming any input.
    *
    * @param output the output to be produced by the parser.
    * @tparam Input the type of the input to be parsed.
    * @tparam Output the type of the output produced by the parser.
    * @return a parser that always succeeds with the given output.
    */
  def unit[Input, Output](output: Output): Parser[Input, Output] =
    input => Success(input, output)

  /** Creates a parser that always fails with the given error message.
    *
    * @param message the error message for the failure.
    * @tparam Input  the type of the input to be parsed.
    * @tparam Output the type of the output produced by the parser.
    * @return a parser that always fails with the given error message.
    */
  def fail[Input, Output](message: String): Parser[Input, Output] =
    input => Failure(ParseError(input, message))
