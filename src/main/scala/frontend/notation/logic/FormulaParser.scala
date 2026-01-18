package proofPlayground
package frontend.notation.logic

import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import frontend.notation.logic.SymbolParser.*
import frontend.notation.parser.Combinators.*
import frontend.notation.parser.Parser
import frontend.notation.{LiteralParser, Tokens}

import scala.language.postfixOps
import scala.reflect.ClassTag

private def variableParser[K: ClassTag] = symbol.Variable.parser[K]

private def trueParser           = symbol.True.parser
private def falseParser          = symbol.False.parser
private def negationParser[F]    = symbol.Negation.parser[F]
private def conjunctionParser[F] = symbol.Conjunction.parser[F]
private def disjunctionParser[F] = symbol.Disjunction.parser[F]
private def implicationParser[F] = symbol.Implication.parser[F]

object FormulaParser:
  extension (self: Parser[Tokens, FormulaF[Formula]])
    private def wrap: Parser[Tokens, Formula] = self.map(Formula(_))

  extension ($: Formula.type)
    def parser: Parser[Tokens, Formula] =
      def implication = FormulaF.Implication.parser(disjunction).wrap
        `orElse` disjunction

      def disjunction = FormulaF.Disjunction.parser(conjunction).wrap
        `orElse` conjunction

      def conjunction = FormulaF.Conjunction.parser(unary).wrap
        `orElse` unary

      def unary = FormulaF.Negation.parser(atomic).wrap
        `orElse` atomic

      def atomic =
        FormulaF.True.parser.wrap
          `orElse` FormulaF.False.parser.wrap
          `orElse` FormulaF.Variable.parser.wrap
          `orElse` parser.between(LiteralParser.parser["("], LiteralParser.parser[")"])
          `orElse` Parser.fail("Exhausted all choices without a match.")

      implication

  extension ($ : FormulaF.Variable.type)
    def parser[T]: Parser[Tokens, FormulaF.Variable[T]] =
      variableParser[FormulaF.Propositional].map(FormulaF.Variable[T])

  extension ($ : FormulaF.True.type)
    def parser[T]: Parser[Tokens, FormulaF.True[T]] =
      trueParser.map(FormulaF.True[T])

  extension ($ : FormulaF.False.type)
    def parser[T]: Parser[Tokens, FormulaF.False[T]] =
      falseParser.map(FormulaF.False[T])

  extension ($ : FormulaF.Negation.type)
    def parser[T](subparser: Parser[Tokens, T]): Parser[Tokens, FormulaF.Negation[T]] =
      negationParser(subparser).map(FormulaF.Negation[T])

  extension ($ : FormulaF.Conjunction.type)
    def parser[T](subparser: Parser[Tokens, T]): Parser[Tokens, FormulaF.Conjunction[T]] =
      conjunctionParser(subparser).map(FormulaF.Conjunction[T])

  extension ($ : FormulaF.Disjunction.type)
    def parser[T](subparser: Parser[Tokens, T]): Parser[Tokens, FormulaF.Disjunction[T]] =
      disjunctionParser(subparser).map(FormulaF.Disjunction[T])

  extension ($ : FormulaF.Implication.type)
    def parser[T](subparser: Parser[Tokens, T]): Parser[Tokens, FormulaF.Implication[T]] =
      implicationParser(subparser).map(FormulaF.Implication[T])
