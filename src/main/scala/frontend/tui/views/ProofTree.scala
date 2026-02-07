package proofPlayground
package frontend.tui.views

import core.logic.propositional.Formula
import frontend.tui.Screen.EventResult
import frontend.tui.models.ProofTreeModel
import frontend.tui.widgets.{ScrollViewState, ScrollViewWidget, Size}
import frontend.tui.{Navigation, Renderer, Screen}
import tree.Tree

import tui.*
import tui.crossterm.{Event, KeyCode, KeyModifiers, MouseEventKind}
import tui.widgets.{BlockWidget, ListWidget, ParagraphWidget}

extension [A](tree: Tree[A])
  private def width: Int =
    if tree.isLeaf then 1
    else
      tree.children.foldLeft(tree.children.length) { (acc, child) => acc.max(child.width) }

  private def height: Int =
    if tree.isLeaf then 1
    else
      tree.children.foldLeft(0) { (acc, child) => acc.max(child.height) } + 1

  private def leaves: Seq[Tree[A]] =
    if tree.isLeaf then Seq(tree)
    else tree.children.flatMap(_.leaves)

extension (view: ScrollViewWidget)
  private def renderer: Renderer = renderer(None)

  private def renderer(cursorRenderer: Option[Renderer]): Renderer = new Renderer:
    override def render(widget: Widget, area: Rect): Unit =
      view.renderWidget(widget, area)

    override def render(widget: StatefulWidget, area: Rect)(state: widget.State): Unit =
      view.renderStatefulWidget(widget, area, state)

    override def setCursor(x: Int, y: Int): Unit =
      cursorRenderer.foreach(_.setCursor(x, y))

object ProofTree:
  def apply(navigation: Navigation)(formula: Formula): ProofTree =
    val model = ProofTreeModel(navigation)(formula)
    new ProofTree(model)(model)

class ProofTree(data: ProofTreeModel.Data)(signals: ProofTreeModel.Signals) extends Screen:
  private val rulesListState  = ListWidget.State()
  private val scrollViewState = ScrollViewState()

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
      case mouse: Event.Mouse if treeFocus =>
        mouse.mouseEvent().kind() match {
          case k: MouseEventKind.ScrollUp =>
            if mouse.mouseEvent().modifiers().bits() == KeyModifiers.ALT then
              scrollViewState.scrollLeft()
            else scrollViewState.scrollUp()
          case k: MouseEventKind.ScrollDown =>
            if mouse.mouseEvent().modifiers().bits() == KeyModifiers.ALT then
              scrollViewState.scrollRight()
            else scrollViewState.scrollDown()
          case _ => EventResult.NotHandled
        }
      case key: Event.Key =>
        key.keyEvent().code() match {
          case c: KeyCode.Enter =>
            if treeFocus then signals.selectNode() else signals.selectRule(rulesListState.selected)
          case c: KeyCode.Esc                => if treeFocus then EventResult.NotHandled else signals.selectRule(None)
          case c: KeyCode.Left               => if treeFocus then signals.left() else EventResult.NotHandled
          case c: KeyCode.Right              => if treeFocus then signals.right() else EventResult.NotHandled
          case c: KeyCode.Up                 => if treeFocus then signals.up() else previousRule()
          case c: KeyCode.Down               => if treeFocus then signals.down() else nextRule()
          case c: KeyCode.Char if c.c == 'q' => if treeFocus then signals.quit() else EventResult.NotHandled
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

    renderScrollableTree(renderer, data.proofTree, layout(1))
    renderRules(renderer, data.rules, layout(0))

  private def renderRules(renderer: Renderer, rules: Vector[ProofTreeModel.ProofRule], area: Rect): Unit =
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

  private def renderScrollableTree(renderer: Renderer, tree: Tree[ProofTreeModel.ProofStep], area: tui.Rect): Unit =
    // alternatively: sum the width of all graphemes in a level and maximize
    val width                   = tree.leaves.length * 40
    val height                  = tree.height * 5
    val verticalScrollbarSize   = if height > area.height then 1 else 0
    val horizontalScrollbarSize = if width > area.width then 1 else 0
    val size = Size(width.max(area.width) - verticalScrollbarSize, height.max(area.height) - horizontalScrollbarSize)
    val scrollView = ScrollViewWidget(size)
    renderTree(scrollView.renderer, data.proofTree, scrollView.area)
    renderer.render(scrollView, area)(scrollViewState)

  private def renderTree(renderer: Renderer, tree: Tree[ProofTreeModel.ProofStep], area: tui.Rect): Unit =
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
      val childrenSizes  = tree.children.map(width)
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
