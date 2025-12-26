package proofPlayground
package core.system.meta

/** General representation of a syntactical inference line.
 *
 * @tparam J The type of judgments in this inference.
 * @param hypotheses The set of hypotheses above the line.
 * @param conclusion The conclusion below the line.
 */
case class Inference[J](hypotheses: Set[J], conclusion: J)
