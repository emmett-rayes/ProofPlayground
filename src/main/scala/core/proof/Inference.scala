package proofPlayground
package core.proof

/** Representation of a syntactical inference line.
  *
  * @tparam J The type of judgments in this inference.
  * @param label      The label of the inference line.
  * @param premises   The sequence of premises above the line.
  * @param conclusion The conclusion below the line.
  */
case class Inference[J](label: String, premises: Seq[J], conclusion: J)
