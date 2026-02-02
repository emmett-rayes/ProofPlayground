package proofPlayground
package core

/** Sequence a [[Seq]] of options with [[Option]]
  * If any of the values in the sequence is None, the result is None.
  * Otherwise, the result is a sequence of all values contained in [[Option]].
  */
def sequence[A](seq: Seq[Option[A]]): Option[Seq[A]] =
  seq.foldLeft(Option(Seq.empty)) { (acc, a) =>
    for b <- a; bs <- acc yield b +: bs
  }

extension [A](seq: Seq[A])
  /** Traverse a sequence of values using a function that produces an option.
    * If any of the values in the sequence maps to None, the result is None.
    * Otherwise, the result is a sequence of all mapped values.
    *
    * @tparam A the type of the consumed values
    * @tparam B the type of the produced values
    * @param seq the sequence of values to traverse
    * @param f the function to apply to each value
    * @return the resulting sequence of produced values ifp all values were produced, None otherwise
    */
  // noinspection ScalaDocUnknownParameter
  def traverse[B](f: A => Option[B]): Option[Seq[B]] =
    sequence[B](seq.map(f))
