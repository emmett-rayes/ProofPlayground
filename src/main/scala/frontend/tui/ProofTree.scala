package proofPlayground
package frontend.tui

import core.logic.propositional.Formula
import frontend.tui.Screen.EventResult
import frontend.tui.models.{ProofRule, ProofStep, ProofTreeModel}
import tree.Tree

import tui.*
import tui.crossterm.{Event, KeyCode}
import tui.widgets.{BlockWidget, ListWidget, ParagraphWidget}

object ProofTree:
  def apply(navigation: Navigation)(formula: Formula): ProofTree =
    val model = ProofTreeModel(navigation)(formula)
    new ProofTree(model)(model)

class ProofTree(data: ProofTreeModel.Data)(signals: ProofTreeModel.Signals) extends Screen:
  private val rulesListState = ListWidget.State()

  override def headerText: Text =
    Text.from(Span.styled("Proof Tree", Style.DEFAULT.fg(Color.Cyan)))

  override def footerText: Text =
    Text.from(
      Span.nostyle("Use "),
      Span.styled("Arrow Keys", Style.DEFAULT.addModifier(Modifier.BOLD)),
      Span.nostyle(" to navigate, "),
      Span.styled("Enter", Style.DEFAULT.addModifier(Modifier.BOLD)),
      Span.nostyle(if data.focusOnRules then " to apply rule" else " to select node"),
      Span.nostyle(", "),
      Span.styled(if data.focusOnRules then "Esc" else "q", Style.DEFAULT.addModifier(Modifier.BOLD)),
      Span.nostyle(" to " + (if data.focusOnRules then "cancel" else "exit") + "."),
    )

  override def handleEvent(event: Event): EventResult =
    import scala.language.implicitConversions
    given Conversion[Unit, EventResult.Handled.type] = _ => EventResult.Handled

    val treeFocus = !data.focusOnRules
    event match {
      case key: tui.crossterm.Event.Key =>
        key.keyEvent().code() match {
          case c: KeyCode.Enter =>
            if treeFocus then signals.selectNode() else signals.selectRule(rulesListState.selected)
          case c: KeyCode.Esc                => if !treeFocus then signals.selectRule(None)
          case c: KeyCode.Left               => signals.left()
          case c: KeyCode.Right              => signals.right()
          case c: KeyCode.Up                 => if treeFocus then signals.up() else previousRule()
          case c: KeyCode.Down               => if treeFocus then signals.down() else nextRule()
          case c: KeyCode.Char if c.c == 'q' => signals.quit()
          case _                             => EventResult.NotHandled
        }
      case _ => EventResult.NotHandled
    }

  private def previousRule(): Unit =
    val i = rulesListState.selected match {
      case Some(i) => if i == 0 then data.rules.length - 1 else i - 1
      case None    => 0
    }
    rulesListState.select(Some(i))

  private def nextRule(): Unit =
    val i = rulesListState.selected match {
      case Some(i) => if i >= data.rules.length - 1 then 0 else i + 1
      case None    => 0
    }
    rulesListState.select(Some(i))

  override def render(renderer: Renderer, area: Rect): Unit =
    val layout = Layout(
      direction = Direction.Horizontal,
      constraints = Array(
        Constraint.Min(25),
        Constraint.Min(0),
      )
    ).split(area)

    if !data.focusOnRules then deselectRule()
    else if rulesListState.selected.isEmpty then nextRule()

    renderRules(renderer, data.rules, layout(0))
    renderTree(renderer, data.proofTree, layout(1))

  private def renderRules(renderer: Renderer, rules: Vector[ProofRule], area: Rect): Unit =
    val items = data.rules.toArray.map { rule =>
      val label = Text.nostyle(rule.rule)
      ListWidget.Item(label, Style(bg = Some(Color.Reset), fg = Some(if rule.active then Color.Green else Color.Gray)))
    }

    val list = ListWidget(
      items = items,
      block = Some(BlockWidget(
        title = Some(Spans.nostyle("Inference Rules")),
        titleAlignment = Alignment.Center,
        borders = Borders.ALL,
        borderStyle = if data.focusOnRules then Style(fg = Some(Color.Yellow)) else Style.DEFAULT
      )),
      highlightStyle = Style(
        bg = Some(Color.LightYellow),
        addModifier = Modifier.BOLD,
      ),
    )

    renderer.render(list, area)(rulesListState)

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
      style = if data.isNodeSelected(tree) then Style.DEFAULT.fg(Color.Yellow) else Style.DEFAULT,
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

  private def deselectRule(): Unit =
    rulesListState.select(None)
