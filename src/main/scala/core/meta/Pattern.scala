package proofPlayground
package core.meta

/** Base trait for patterns appearing mostly in rules of inference.
 *
 * Patterns allow matching against proof structures using either
 * meta-variables (placeholders) or concrete values, or a combination.
 */
sealed trait Pattern

case object Pattern:

  /** Pattern for matching formulas in proof structures.
   *
   * A Formula pattern can be either a meta-variable that matches
   * any formula or a concrete formula.
   *
   * @tparam X the formula functor of the referenced formulas.
   */
  enum Formula[X[_]] extends Pattern:

    /** A meta-variable pattern that matches any formula.
     *
     * @param name the identifier for this meta-variable.
     */
    case Meta[F[_]](name: String) extends Formula[F]

    /** A concrete formula pattern that matches a specific formula.
     *
     * @tparam F the formula functor of the concrete formula.
     * @param formula the specific formula to match.
     */
    case Concrete[F[_]](formula: F[Formula[F]]) extends Formula[F]

  case object Formula:

    /** Implicit conversion from `String` to `Formula.Meta`.
     *
     * Enables using a plain identifier as a meta-variable in the pattern DSL,
     * e.g. "A" becomes `Formula.Meta[F]("A")` for any formula functor `F`.
     *
     * @tparam F the formula functor of the referenced formulas.
     * @return a `Formula.Meta[F]` constructed from the given string name.
     */
    given [F[_]] => Conversion[String, Meta[F]] = Meta(_)

    /** Implicit conversion from a concrete formula to `Formula.Concrete`.
     *
     * Allows writing a concrete formula directly in the pattern DSL,
     * e.g. a value of type `F[Formula[F]]` becomes `Formula.Concrete[F](value)`.
     *
     * @tparam F the formula functor of the concrete formula.
     * @return a `Formula.Concrete[F]` wrapping the provided concrete formula.
     */
    given [F[_]] => Conversion[F[Formula[F]], Concrete[F]] = Concrete(_)

  /** Pattern for matching sequences in proof structures.
   *
   * A Seq pattern can be either a meta-variable that matches any sequence
   * or a concrete sequence of patterns.
   *
   * @tparam S the type of sequence elements (contravariant).
   */
  enum Seq[-S] extends Pattern:

    /** A meta-variable pattern that matches any sequence.
     *
     * @param name the identifier for this meta-variable.
     */
    case Meta(name: String) extends Seq[Pattern]

    /** A concrete sequence pattern that matches a specific sequence of patterns.
     *
     * @param seq the sequence of patterns to match.
     */
    case Concrete(seq: scala.Seq[Pattern]) extends Seq[Pattern]

  case object Seq:

    /** Implicit conversion from `String` to `Seq.Meta`.
     *
     * Allows using plain identifiers for sequence meta-variables in the DSL,
     * e.g. "Gamma" becomes `Seq.Meta("Gamma")`.
     *
     * @return a conversion that creates a `Seq.Meta` pattern from a `String` name.
     */
    given Conversion[String, Meta] = Meta(_)

    /** Extension methods for sequence patterns.
     *
     * Provides DSL for constructing sequence patterns.
     */
    extension [S](seq: Seq[S])

      /** Prepend a pattern to an existing sequence pattern, producing a concrete sequence.
       *
       * Useful for building sequence patterns incrementally.
       *
       * @param pattern the pattern to prepend.
       * @return a `Seq.Concrete` containing the existing `seq` followed by `pattern`.
       */
      def ::(pattern: Pattern) = Concrete(scala.Seq(seq, pattern))
