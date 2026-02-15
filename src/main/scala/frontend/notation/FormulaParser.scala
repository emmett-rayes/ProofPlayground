package proofPlayground
package frontend.notation

import core.logic.propositional.Formula.given
import core.logic.propositional.FormulaF.*
import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import parser.Combinators.*
import parser.{LiteralParser, Parser, Tokens}
import frontend.notation.SymbolParser.*

import scala.language.{implicitConversions, postfixOps}
import scala.reflect.ClassTag

private def variableParser[K: ClassTag] = symbol.Variable.parser[K]

private def trueParser           = symbol.True.parser
private def falseParser          = symbol.False.parser
private def negationParser[F]    = symbol.Negation.parser[F]
private def conjunctionParser[F] = symbol.Conjunction.parser[F]
private def disjunctionParser[F] = symbol.Disjunction.parser[F]
private def implicationParser[F] = symbol.Implication.parser[F]

private def negationChainedParser[F]    = symbol.Negation.chainedParser[F]
private def conjunctionChainedParser[F] = symbol.Conjunction.chainedParser[F]
private def disjunctionChainedParser[F] = symbol.Disjunction.chainedParser[F]
private def implicationChainedParser[F] = symbol.Implication.chainedParser[F]

object FormulaParser:

  extension (self: Parser[Tokens, FormulaF[Formula]])
    private def wrap: Parser[Tokens, Formula] = self.map(Formula(_))

  extension ($ : Formula.type)
    def parser: Parser[Tokens, Formula] =

      def implication: Parser[Tokens, Formula] =
        FormulaF.Implication.chainedParser(disjunction).wrap
          `orElse` disjunction

      def disjunction: Parser[Tokens, Formula] =
        FormulaF.Disjunction.chainedParser(conjunction).wrap
          `orElse` conjunction

      def conjunction: Parser[Tokens, Formula] =
        FormulaF.Conjunction.chainedParser(unary).wrap
          `orElse` unary

      def unary: Parser[Tokens, Formula] =
        val recurse : Parser[Tokens, Formula] = tokens => unary.parse(tokens)
        FormulaF.Negation.chainedParser(recurse).wrap
          `orElse` atomic

      def atomic: Parser[Tokens, Formula] =
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
      
    def chainedParser[T](subparser: Parser[Tokens, T])(using
      Conversion[FormulaF.Negation[T], T]
    ) : Parser[Tokens, FormulaF.Negation[T]] =
      negationChainedParser(subparser)(FormulaF.Negation(_)).map(FormulaF.Negation[T])

  extension ($ : FormulaF.Conjunction.type)
    def parser[T](subparser: Parser[Tokens, T]): Parser[Tokens, FormulaF.Conjunction[T]] =
      conjunctionParser(subparser).map(FormulaF.Conjunction[T])

    def chainedParser[T](subparser: Parser[Tokens, T])(using
      Conversion[FormulaF.Conjunction[T], T]
    ): Parser[Tokens, FormulaF.Conjunction[T]] =
      conjunctionChainedParser(subparser)(FormulaF.Conjunction(_)).map(FormulaF.Conjunction[T])

  extension ($ : FormulaF.Disjunction.type)
    def parser[T](subparser: Parser[Tokens, T]): Parser[Tokens, FormulaF.Disjunction[T]] =
      disjunctionParser(subparser).map(FormulaF.Disjunction[T])

    def chainedParser[T](subparser: Parser[Tokens, T])(using
      Conversion[FormulaF.Disjunction[T], T]
    ): Parser[Tokens, FormulaF.Disjunction[T]] =
      disjunctionChainedParser(subparser)(FormulaF.Disjunction(_)).map(FormulaF.Disjunction[T])

  extension ($ : FormulaF.Implication.type)
    def parser[T](subparser: Parser[Tokens, T]): Parser[Tokens, FormulaF.Implication[T]] =
      implicationParser(subparser).map(FormulaF.Implication[T])

    def chainedParser[T](subparser: Parser[Tokens, T])(using
      Conversion[FormulaF.Implication[T], T]
    ): Parser[Tokens, FormulaF.Implication[T]] =
      implicationChainedParser(subparser)(FormulaF.Implication(_)).map(FormulaF.Implication[T])
