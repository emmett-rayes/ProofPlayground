package proofPlayground
package core.proof

import core.meta.Pattern

import scala.quoted.{Expr, Quotes, Type}

object ObjectSeq:
  /** Extracts inference rules from an object. */
  inline def objectSeq[J[_], F[_]](obj: Any): Seq[Inference[J[Pattern[F]]]] =
    ${ objectSeqImpl('obj) }

  private def objectSeqImpl[J[_], F[_]](obj: Expr[Any])(using
    Type[J],
    Type[F]
  )(using quotes: Quotes): Expr[Seq[Inference[J[Pattern[F]]]]] =
    import quotes.reflect.*

    val tpe           = obj.asTerm.tpe
    val members       = tpe.typeSymbol.fieldMembers
    val inferenceType = TypeRepr.of[Inference[J[Pattern[F]]]]

    val exprs =
      members.filter { sym =>
        sym.isValDef && tpe.memberType(sym) <:< inferenceType
      }.map {
        sym => Select(obj.asTerm, sym).asExprOf[Inference[J[Pattern[F]]]]
      }

    Expr.ofSeq(exprs)
