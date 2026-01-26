package proofPlayground
package frontend.tui

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ParagraphWidget}

object ProofTree:
  def apply(navigation: Navigation): ProofTree =
    val model = ProofModeModel(navigation)
    new ProofTree(model)(model)

class ProofTree(data: ProofModeModel.Data)(signals: ProofModeModel.Signals) extends Screen:
  override def headerText: Text =
    Text.from(Span.styled("Proof Tree", Style.DEFAULT.fg(Color.Cyan)))

  override def footerText: Text =
    Text.from(
      Span.nostyle("Press "),
      Span.styled("q", Style.DEFAULT.addModifier(Modifier.BOLD)),
      Span.nostyle(" to exit.")
    )

  override def handleEvent(event: Event): Unit =
    event match {
      case key: tui.crossterm.Event.Key =>
        key.keyEvent().code() match {
          case c: KeyCode.Char if c.c == 'q' => signals.quit()
          case _                             => ()
        }
      case _ => ()
    }

  override def render(renderer: Renderer, area: Rect): Unit =
    val chunks = Layout(
      direction = Direction.Vertical,
      margin = Margin(1),
      constraints = Array(
        Constraint.Length(2), // header
        Constraint.Min(0),    // proof tree
        Constraint.Length(2), // footer
      ),
    ).split(area)

    renderTree(renderer, data.proofTree, chunks(1))

  private def renderTree(renderer: Renderer, tree: Tree[String], area: tui.Rect): Unit =
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
      block = Some(BlockWidget(borders = Borders.TOP)),
    )

    val nodeWidget = tree match
      case Tree.Leaf(value) =>
        ParagraphWidget(text = Text.from(Span.nostyle(value)), alignment = Alignment.Center)

      case Tree.Node(value, children) =>
        if children.nonEmpty then
          val childrenLayout = Layout(
            direction = Direction.Horizontal,
            constraints = Array.fill(children.length)(Constraint.Ratio(1, children.length)),
          ).split(nodeLayout(0))

          for (child, idx) <- children.zipWithIndex do
            renderTree(renderer, child, childrenLayout(idx))
        end if

        ParagraphWidget(text = Text.from(Span.nostyle(value)), alignment = Alignment.Center)

    renderer.render(divider, nodeLayout(1))
    renderer.render(nodeWidget, nodeLayout.last)
