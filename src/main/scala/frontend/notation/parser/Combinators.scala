package proofPlayground
package frontend.notation.parser

object Combinators:
  extension [Input, Output](self: => Parser[Input, Output])
    /** Maps the output of this parser if successful using the given function `f`.
      *
      * @param f a function that takes the output of this parser and returns a mapped value.
      * @tparam Mapped the type of the output produced by the resulting parser.
      * @return a new parser that applies this parser and then maps its output using `f`.
      */
    def map[Mapped](f: Output => Mapped): Parser[Input, Mapped] =
      self.flatMap(output => Parser.unit(f(output)))

    /** Parses zero or more occurrences of this parser, collecting the results into a list.
      *
      * @return a new parser that produces a list of outputs from zero or more applications of this parser.
      */
    def repeated: Parser[Input, List[Output]] =
      val greedy =
        for selfOutput <- self; otherOutputs <- repeated
        yield selfOutput :: otherOutputs
      greedy.orElse(Parser.unit(List())).map(_.merge)

    /** Parses `n` or more occurrences of this parser, collecting the results into a list.
      *
      * @param n the minimum number of occurrences to parse.
      * @return a new parser that produces a list of outputs from `n` or more applications of this parser.
      */
    def atLeast(n: Int): Parser[Input, List[Output]] =
      self.repeated.flatMap { outputs =>
        if outputs.size >= n then Parser.unit(outputs)
        else Parser.fail(s"Expected at least ${n - outputs.size} more element(s) after ${outputs.mkString}.")
      }

    /** Chains this parser with another parser, producing a tuple of their outputs.
      *
      * @param other the other parser to apply after this parser.
      * @tparam Then the type of the output produced by the other parser.
      * @return a new parser that produces a tuple of the outputs from this parser and the other parser.
      */
    def andThen[Then](other: => Parser[Input, Then]): Parser[Input, (Output, Then)] =
      for selfOutput <- self; otherOutput <- other
      yield (selfOutput, otherOutput)

    /** Chains this parser with another parser, discarding the output of this parser.
      *
      * @param other the other parser to apply after this parser.
      * @tparam Then the type of the output produced by the other parser.
      * @return a new parser that produces the output from the other parser.
      */
    def skipThen[Then](other: => Parser[Input, Then]): Parser[Input, Then] =
      self.andThen(other).map(_._2)

    /** Chains this parser with another parser, discarding the output of the other parser.
      *
      * @param other the other parser to apply after this parser.
      * @tparam Skip the type of the output produced by the other parser.
      * @return a new parser that produces the output from this parser.
      */
    def thenSkip[Skip](other: => Parser[Input, Skip]): Parser[Input, Output] =
      self.andThen(other).map(_._1)

    /** Parses this parser between the outputs of two other parsers, discarding their outputs.
      *
      * @param first  the parser to apply before this parser.
      * @param second the parser to apply after this parser.
      * @tparam First  the type of the output produced by the first parser.
      * @tparam Second the type of the output produced by the second parser.
      * @return a new parser that produces the output from this parser.
      */
    def between[First, Second](
      first: => Parser[Input, First],
      second: => Parser[Input, Second]
    ): Parser[Input, Output] =
      first.skipThen(self).thenSkip(second)
