package proofPlayground
package core

/** A type class representing a functor.
 *
 * A functor is a type constructor `F[_]` that supports a mapping operation
 * that applies a function to values inside the context of `F`.
 *
 * @tparam F The functor type constructor.
 */
trait Functor[F[_]]:
  /** Extension methods for values of type `F[A]`.
   *
   * @tparam A The type of values inside the functor.
   * @param fa The functor instance
   */
  extension [A](fa: F[A])
    /** Map a function over the functor.
     *
     * @param f The function to apply to values inside the functor.
     * @tparam B The result type of the function.
     * @return A new functor with the function applied to its values.
     */
    def map[B](f: A => B): F[B]
