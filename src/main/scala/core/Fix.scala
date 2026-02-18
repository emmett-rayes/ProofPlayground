package proofPlayground
package core

/** A fixed-point type constructor for recursive data structures.
  *
  * @tparam F A type constructor that takes one type parameter.
  */
case class Fix[F[_]](unfix: F[Fix[F]])

extension [F[_]](self: F[Fix[F]]) {
  def fix: Fix[F] = Fix(self)
}
