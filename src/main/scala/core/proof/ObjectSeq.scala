package proofPlayground
package core.proof

import scala.quoted.{Expr, Quotes, Type}

object ObjectSeq {
  /** A macro that extracts all inference rules defined as values in an object.
    *
    * This is a helper macro that saves us from manually listing all inference rules in the proof system twice.
    */
  inline def objectSeq[J[_], F[_]](obj: Any): Seq[InferenceRule[J, F]] =
    ${ objectSeqImpl('obj) }

  /** Macro implementation of [[objectSeq]]. */
  private def objectSeqImpl[J[_], F[_]](obj: Expr[Any])(using
    Type[J],
    Type[F]
  )(using quotes: Quotes): Expr[Seq[InferenceRule[J, F]]] = {
    import quotes.reflect.*

    val tpe           = obj.asTerm.tpe
    val members       = tpe.typeSymbol.fieldMembers
    val inferenceType = TypeRepr.of[InferenceRule[J, F]]

    val exprs =
      members.filter { sym =>
        sym.isValDef && tpe.memberType(sym) <:< inferenceType
      }.map {
        sym => Select(obj.asTerm, sym).asExprOf[InferenceRule[J, F]]
      }

    Expr.ofSeq(exprs)
  }
}
