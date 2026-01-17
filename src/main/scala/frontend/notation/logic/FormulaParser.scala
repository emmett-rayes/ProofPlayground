package proofPlayground
package frontend.notation.logic

import core.logic.propositional.{Formula, FormulaF}
import core.logic.symbol
import frontend.notation.Tokens
import frontend.notation.logic.SymbolParser.*
import frontend.notation.parser.Combinators.map
import frontend.notation.parser.Parser

import scala.reflect.ClassTag

private def variableParser[K: ClassTag] = symbol.Variable.parser[K]

private def trueParser           = symbol.True.parser
private def falseParser          = symbol.False.parser
private def negationParser[F]    = symbol.Negation.parser[F]
private def conjunctionParser[F] = symbol.Conjunction.parser[F]
private def disjunctionParser[F] = symbol.Disjunction.parser[F]
private def implicationParser[F] = symbol.Implication.parser[F]

object FormulaParser:
  extension [T]($ : Formula)(using Conversion[T, Formula])(using p: Parser[Tokens, T])
    def parser: Parser[Tokens, Formula] =
      p.map(r => r)

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
