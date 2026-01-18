package proofPlayground
package frontend.notation

import frontend.notation.parser.{ParseError, Parser, ParserResult}

import scala.collection.mutable
import scala.language.reflectiveCalls
import scala.reflect.ClassTag
import scala.reflect.Selectable.reflectiveSelectable
import scala.util.Failure

object NonRecursiveParser:
  opaque type Context = mutable.Set[(Int, ClassTag[?])]

  object Context:
    def apply(): Context = mutable.Set.empty

  extension [Input <: { def size: Int }, Output: ClassTag](self: => Parser[Input, Output])
    /** Creates a non-recursive version of this parser that rejects left-recursion.
      *
      * This is a convenience parser that helps avoid having to refactor left-recursive grammars.
      *
      * This parser keeps track of the inputs it is currently parsing for each output type.
      * If it encounters the same input size and output type again while parsing, it
      * indicates non-productive left-recursion and fails with a parse error.
      *
      * @param context the context to use for tracking left-recursion, which is shared among mutually recursive parsers.
      * @return a parser that fails if left-recursion is detected.
      */
    def nonRecur(using context: Context): Parser[Input, Output] =
      new Parser[Input, Output]:
        private val pending: Context = context

        override def parse(input: Input): ParserResult[Input, Output] =
          val tag = summon[ClassTag[Output]]
          val key = (input.size, tag)
          if pending.contains(key)
          then Failure(ParseError(input, "Left-recursion detected."))
          else
            pending += key
            val result = self.parse(input)
            pending -= key
            result
