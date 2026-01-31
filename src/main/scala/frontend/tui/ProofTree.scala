package proofPlayground
package frontend.tui

import core.logic.propositional.Formula
import frontend.tui.models.{ProofStep, ProofTreeModel}
import tree.Tree

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ParagraphWidget}

object ProofTree:
  def apply(navigation: Navigation)(formula: Formula): ProofTree =
    val model = ProofTreeModel(navigation)(formula)
    new ProofTree(model)(model)

class ProofTree(data: ProofTreeModel.Data)(signals: ProofTreeModel.Signals) extends Screen:
  override def headerText: Text =
    Text.from(Span.styled("Proof Tree", Style.DEFAULT.fg(Color.Cyan)))

  override def footerText: Text =
    Text.from(
      Span.nostyle("Press "),
      Span.styled("Enter", Style.DEFAULT.addModifier(Modifier.BOLD)),
      Span.nostyle(" to select, "),
      Span.styled("q", Style.DEFAULT.addModifier(Modifier.BOLD)),
      Span.nostyle(" to exit.")
    )

  override def handleEvent(event: Event): Unit =
    event match {
      case key: tui.crossterm.Event.Key =>
        key.keyEvent().code() match {
          case c: KeyCode.Char if c.c == 'q' => signals.quit()
          case c: KeyCode.Up                 => signals.up()
          case c: KeyCode.Down               => signals.down()
          case c: KeyCode.Left               => signals.left()
          case c: KeyCode.Right              => signals.right()
          case c: KeyCode.Enter              => signals.select()
          case _                             => ()
        }
      case _ => ()
    }

  override def render(renderer: Renderer, area: Rect): Unit =
    renderTree(renderer, data.proofTree, area)

  private def renderTree(renderer: Renderer, tree: Tree[ProofStep], area: tui.Rect): Unit =
    val nodeLayout = Layout(
      direction = Direction.Vertical,
      margin = Margin(0, 1),
      constraints = Array(
        Constraint.Min(0),    // children
        Constraint.Length(1), // divider
        Constraint.Length(3), // node
      ),
    ).split(area)

    val divider = ParagraphWidget(
      text = Text.from(Span.nostyle("")),
      block =
        Some(BlockWidget(
          borders = Borders.TOP,
          title = Some(Spans.nostyle(s"[${tree.value.rule}]")),
          titleAlignment = Alignment.Right,
        )),
    )

    val nodeWidget = ParagraphWidget(
      text = Text.from(Span.nostyle(tree.value.formula)),
      alignment = Alignment.Center,
      style = if data.nodeSelected(tree) then Style.DEFAULT.fg(Color.Yellow) else Style.DEFAULT,
    )

    renderer.render(divider, nodeLayout(1))
    renderer.render(nodeWidget, nodeLayout.last)

    // render children
    if !tree.isLeaf then
      val childrenSizes = tree.children.map { tree =>
        def width(tree: Tree[?]): Int =
          if tree.isLeaf then 1
          else
            tree.children.foldLeft(tree.children.length) { (acc, child) =>
              acc.max(width(child))
            }
        width(tree)
      }

      val childrenLayout = Layout(
        direction = Direction.Horizontal,
        constraints = Array.tabulate(tree.children.length) { idx =>
          Constraint.Ratio(childrenSizes(idx), childrenSizes.sum)
        },
      ).split(nodeLayout(0))

      for (child, idx) <- tree.children.zipWithIndex do
        renderTree(renderer, child, childrenLayout(idx))
