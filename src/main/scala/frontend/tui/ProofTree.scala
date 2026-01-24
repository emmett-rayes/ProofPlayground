package proofPlayground
package frontend.tui

import tui.*
import tui.crossterm.Event
import tui.widgets.{BlockWidget, ParagraphWidget}

object ProofTree:
  def apply(): ProofTree =
    val model = ProofModeModel()
    new ProofTree(model)

class ProofTree(data: ProofModeModel.Data) extends Screen:
  override def handleEvent(event: Event): Unit = ()

  override def render(frame: Frame): Unit =
    val chunks = Layout(
      direction = Direction.Vertical,
      margin = Margin(1),
      constraints = Array(
        Constraint.Length(2), // header
        Constraint.Min(0),    // proof tree
        Constraint.Length(2), // footer
      ),
    ).split(frame.size)

    val header = ParagraphWidget(
      text = Text.from(Span.styled("Proof Tree", Style.DEFAULT.fg(Color.Cyan))),
      block = Some(BlockWidget(borders = Borders.BOTTOM)),
    )

    val footer = ParagraphWidget(
      text = Text.from(
        Span.nostyle("Press "),
        Span.styled("q", Style.DEFAULT.addModifier(Modifier.BOLD)),
        Span.nostyle(" to exit.")
      ),
      block = Some(BlockWidget(borders = Borders.TOP)),
    )

    frame.renderWidget(header, chunks(0))
    renderTree(frame, data.proofTree, chunks(1))
    frame.renderWidget(footer, chunks.last)

  private def renderTree(frame: Frame, tree: Tree[String], area: tui.Rect): Unit =
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
            renderTree(frame, child, childrenLayout(idx))
        end if

        ParagraphWidget(text = Text.from(Span.nostyle(value)), alignment = Alignment.Center)

    frame.renderWidget(divider, nodeLayout(1))
    frame.renderWidget(nodeWidget, nodeLayout.last)
