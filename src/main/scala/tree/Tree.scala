package proofPlayground
package tree

enum Tree[+A]:
  case Leaf(value: A)
  case Node(value: A, children: List[Tree[A]])

