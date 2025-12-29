package proofPlayground
package core

/** An algebra for a functor `F` producing values of type `A`.
  *
  * @tparam F The functor type constructor.
  * @tparam A The result type of the algebra.
  */
type Algebra[F[_], A] = F[A] => A

/** Perform a catamorphism (generalized fold) over a fixed-point structure over a functor.
  *
  * @tparam F The functor type constructor.
  * @tparam A The result type of the catamorphism.
  * @param fix The fixed-point structure (initial f-algebra) to fold over.
  * @param algebra The algebra defining how to reduce the functor's structure into a value of type `A`.
  * @return The result of folding the fixed-point structure into a value of type `A`.
  */
def catamorphism[F[_]: Functor, A, B](fix: Fix[F])(algebra: Algebra[F, A]): A =
  algebra(fix.unfix.map(a => catamorphism(a)(algebra)))
