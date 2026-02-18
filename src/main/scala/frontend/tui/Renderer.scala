package proofPlayground
package frontend.tui

import tui.{Rect, StatefulWidget, Widget}

trait Renderer {
  def render(widget: Widget, area: Rect): Unit
  def render(widget: StatefulWidget, area: Rect)(state: widget.State): Unit
  def setCursor(x: Int, y: Int): Unit
}
