package proofPlayground
package frontend.notation

import core.logic.symbol.*
import parser.Combinators.*
import parser.{LiteralParser, Parser, RegexParser, Tokens}

import scala.reflect.ClassTag

object SymbolParser:
  extension ($ : Variable.type)
    /** Parser for variable symbols.
      *
      * @tparam K the type of the variable identifiers.
      * @return a parser that recognizes and produces propositional variables.
      */
    def parser[K: ClassTag]: Parser[Tokens, Variable[K]] =
      RegexParser.parser("[A-Z]\\d*".r).map(s => Variable[K](s))

  extension ($ : True.type)
    /** Parser for the true constant.
      *
      * @return a parser that recognizes and produces the True constant.
      */
    def parser: Parser[Tokens, True] =
      val op = LiteralParser.parser["True"].orElse(LiteralParser.parser["⊤"])
      op.map(_ => True())

  extension ($ : False.type)
    /** Parser for the false constant.
      *
      * @return a parser that recognizes and produces the True constant.
      */
    def parser: Parser[Tokens, False] =
      val op = LiteralParser.parser["False"].orElse(LiteralParser.parser["⊥"])
      op.map(_ => False())

  extension ($ : Negation.type)
    /** Parser for unary negation.
      *
      * @param subparser the parser for the subformula.
      * @return a parser that recognizes and produces negations.
      */
    def parser[F](subparser: Parser[Tokens, F]): Parser[Tokens, Negation[F]] =
      val op = LiteralParser.parser["~"].orElse(LiteralParser.parser["¬"])
      op.skipThen(subparser).map(Negation(_))

    /** Parser for chained negations.
      *
      * @param subparser the parser for the subformula.
      * @param converter a function to convert a negation of a formula into a formula.
      * @return a parser that recognizes and produces a list of negations as right-associated negations.
      */
    def chainedParser[F](subparser: Parser[Tokens, F])(converter: Negation[F] => F): Parser[Tokens, Negation[F]] =
      val op = LiteralParser.parser["~"].orElse(LiteralParser.parser["¬"])
      op.atLeast(1).andThen(subparser).map { (negations, formula) =>
        val arg = negations.drop(1).foldRight(formula) { (_, acc) => converter(Negation(acc)) }
        Negation(arg)
      }

  extension ($ : Conjunction.type)
    /** Parser for binary conjunction.
      *
      * @param subparser the parser for the subformulas.
      * @return a parser that recognizes and produces binary conjunctions.
      */
    def parser[F](subparser: Parser[Tokens, F]): Parser[Tokens, Conjunction[F]] =
      val op = LiteralParser.parser["/\\"].orElse(LiteralParser.parser["∧"])
      subparser.thenSkip(op).andThen(subparser).map {
        (lhs, rhs) => Conjunction(lhs, rhs)
      }

    /** Parser for chained conjunctions.
      *
      * @param subparser the parser for the subformulas.
      * @param converter a function to convert a conjunction of two subformulas into a single formula.
      * @return a parser that recognizes and produces lists of subformulas as left-associated conjunctions.
      */
    def chainedParser[F](subparser: Parser[Tokens, F])(converter: Conjunction[F] => F): Parser[Tokens, Conjunction[F]] =
      val op = LiteralParser.parser["/\\"].orElse(LiteralParser.parser["∧"])
      subparser.andThen(op.skipThen(subparser).atLeast(1)).map { (first, rest) =>
        val lhs = rest.dropRight(1).foldLeft(first)((acc, curr) => converter(Conjunction(acc, curr)))
        Conjunction(lhs, rest.last)
      }

  extension ($ : Disjunction.type)
    /** Parser for binary disjunction.
      *
      * @param subparser the parser for the subformulas.
      * @return a parser that recognizes and produces binary disjunction.
      */
    def parser[F](subparser: Parser[Tokens, F]): Parser[Tokens, Disjunction[F]] =
      val op = LiteralParser.parser["\\/"].orElse(LiteralParser.parser["∨"])
      subparser.thenSkip(op).andThen(subparser).map {
        (lhs, rhs) => Disjunction(lhs, rhs)
      }

    /** Parser for chained disjunctions.
      *
      * @param subparser the parser for the subformulas.
      * @param converter a function to convert a disjunction of two subformulas into a single formula.
      * @return a parser that recognizes and produces lists of subformulas as left-associated disjunctions.
      */
    def chainedParser[F](subparser: Parser[Tokens, F])(converter: Disjunction[F] => F): Parser[Tokens, Disjunction[F]] =
      val op = LiteralParser.parser["\\/"].orElse(LiteralParser.parser["∨"])
      subparser.andThen(op.skipThen(subparser).atLeast(1)).map { (first, rest) =>
        val lhs = rest.dropRight(1).foldLeft(first)((acc, curr) => converter(Disjunction(acc, curr)))
        Disjunction(lhs, rest.last)
      }

  extension ($ : Implication.type)
    /** Parser for implication.
      *
      * @param subparser the parser for the subformulas.
      * @return a parser that recognizes and produces implication.
      */
    def parser[F](subparser: Parser[Tokens, F]): Parser[Tokens, Implication[F]] =
      val op = LiteralParser.parser["-->"].orElse(LiteralParser.parser["->"].orElse(LiteralParser.parser["→"]))
      subparser.thenSkip(op).andThen(subparser).map {
        (lhs, rhs) => Implication(lhs, rhs)
      }

    /** Parser for chained implications.
      *
      * @param subparser the parser for the subformulas.
      * @param converter a function to convert an implication of two subformulas into a single formula.
      * @return a parser that recognizes and produces lists of subformulas as right-associated implications.
      */
    def chainedParser[F](subparser: Parser[Tokens, F])(converter: Implication[F] => F): Parser[Tokens, Implication[F]] =
      val op = LiteralParser.parser["-->"].orElse(LiteralParser.parser["->"].orElse(LiteralParser.parser["→"]))
      subparser.andThen(op.skipThen(subparser).atLeast(1)).map { (first, rest) =>
        val rhs = rest.reduceRight((curr, acc) => converter(Implication(curr, acc)))
        Implication(first, rhs)
      }

  extension ($ : Difference.type)
    /** Parser for difference.
      *
      * @param subparser the parser for the subformulas.
      * @return a parser that recognizes and produces difference.
      */
    def parser[F](subparser: Parser[Tokens, F]): Parser[Tokens, Difference[F]] =
      val op = LiteralParser.parser["--<"].orElse(LiteralParser.parser["-<"])
      subparser.thenSkip(op).andThen(subparser).map {
        (lhs, rhs) => Difference(lhs, rhs)
      }

    /** Parser for chained differences.
      *
      * @param subparser the parser for the subformulas.
      * @param converter a function to convert a difference of two subformulas into a single formula.
      * @return a parser that recognizes and produces lists of subformulas as right-associated differences.
      */
    def chainedParser[F](subparser: Parser[Tokens, F])(converter: Difference[F] => F): Parser[Tokens, Difference[F]] =
      val op = LiteralParser.parser["--<"].orElse(LiteralParser.parser["-<"])
      subparser.andThen(op.skipThen(subparser).atLeast(1)).map { (first, rest) =>
        val rhs = rest.reduceRight((curr, acc) => converter(Difference(curr, acc)))
        Difference(first, rhs)
      }

  extension ($ : Universal.type)
    /** Parser for universal quantification.
      *
      * @param varparser the parser for the quantification variable.
      * @param subparser the parser for the body of the quantification.
      * @return a parser that recognizes and produces universal quantifications.
      */
    def parser[V, F](varparser: Parser[Tokens, V], subparser: Parser[Tokens, F]): Parser[Tokens, Universal[V, F]] =
      val op = LiteralParser.parser["forall"].orElse(LiteralParser.parser["∀"])
      op.skipThen(varparser).thenSkip(LiteralParser.parser["."]).andThen(subparser).map {
        (variable, body) => Universal(variable, body)
      }

  extension ($ : Existential.type)
    /** Parser for existential quantification.
      *
      * @param varparser the parser for the quantification variable.
      * @param subparser the parser for the body of the quantification.
      * @return a parser that recognizes and produces existential quantifications.
      */
    def parser[V, F](varparser: Parser[Tokens, V], subparser: Parser[Tokens, F]): Parser[Tokens, Existential[V, F]] =
      val op = LiteralParser.parser["exists"].orElse(LiteralParser.parser["∃"])
      op.skipThen(varparser).thenSkip(LiteralParser.parser["."]).andThen(subparser).map {
        (variable, body) => Existential(variable, body)
      }
