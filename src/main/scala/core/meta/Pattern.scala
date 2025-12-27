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
    given [F[_]]: Conversion[F[Formula[F]], Concrete[F]] = Concrete(_)

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
