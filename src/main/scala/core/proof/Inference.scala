package proofPlayground
package core.proof

/** Representation of a syntactical inference line.
  *
  * @tparam J The type of judgments in this inference.
  * @param label      The label of the inference line.
  * @param hypotheses The set of hypotheses above the line.
  * @param conclusion The conclusion below the line.
  */
case class Inference[J](label: String)(hypotheses: Set[J], conclusion: J)
