package proofPlayground
package frontend.notation.logic

import core.logic.symbol.*
import frontend.notation.parser.Combinators.*
import frontend.notation.parser.{Literal, Parser, Tokens}

object SymbolParser:
  extension ($ : True.type)
    /** Parser for the true constant.
      *
      * @return a parser that recognizes and produces the True constant.
      */
    def parser: Parser[Tokens, True] =
      Literal.parser["True"].orElse(Literal.parser["⊤"]).map(_ => True())

  extension ($ : False.type)
    /** Parser for the false constant.
      *
      * @return a parser that recognizes and produces the True constant.
      */
    def parser: Parser[Tokens, False] =
      Literal.parser["False"].orElse(Literal.parser["⊥"]).map(_ => False())

  extension ($ : Negation.type)
    /** Parser for unary negation.
      *
      * @param subparser the parser for the subformula.
      * @return a parser that recognizes and produces negations.
      */
    def parser[F](subparser: Parser[Tokens, F]): Parser[Tokens, Negation[F]] =
      Literal.parser["~"].orElse(Literal.parser["¬"]).skipThen(subparser).map(Negation(_))

  extension ($ : Conjunction.type)
    /** Parser for binary conjunction.
      *
      * @param subparser the parser for the subformulas.
      * @return a parser that recognizes and produces binary conjunctions.
      */
    def parser[F](subparser: Parser[Tokens, F]): Parser[Tokens, Conjunction[F]] =
      subparser.thenSkip(Literal.parser["/\\"].orElse(Literal.parser["∧"])).andThen(subparser).map {
        (lhs, rhs) => Conjunction(lhs, rhs)
      }

  extension ($ : Disjunction.type)
    /** Parser for binary disjunction.
      *
      * @param subparser the parser for the subformulas.
      * @return a parser that recognizes and produces binary disjunction.
      */
    def parser[F](subparser: Parser[Tokens, F]): Parser[Tokens, Disjunction[F]] =
      subparser.thenSkip(Literal.parser["\\/"].orElse(Literal.parser["∨"])).andThen(subparser).map {
        (lhs, rhs) => Disjunction(lhs, rhs)
      }

  extension ($ : Implication.type)
    /** Parser for implication.
      *
      * @param subparser the parser for the subformulas.
      * @return a parser that recognizes and produces implication.
      */
    def parser[F](subparser: Parser[Tokens, F]): Parser[Tokens, Implication[F]] =
      subparser.thenSkip(Literal.parser["-->"].orElse(Literal.parser["→"])).andThen(subparser).map {
        (lhs, rhs) => Implication(lhs, rhs)
      }

  extension ($ : Difference.type)
    /** Parser for difference.
      *
      * @param subparser the parser for the subformulas.
      * @return a parser that recognizes and produces difference.
      */
    def parser[F](subparser: Parser[Tokens, F]): Parser[Tokens, Difference[F]] =
      subparser.thenSkip(Literal.parser["--<"]).andThen(subparser).map {
        (lhs, rhs) => Difference(lhs, rhs)
      }
