package proofPlayground
package frontend.tui

import tui.{Rect, StatefulWidget, Widget}

trait Renderer:
  def renderWidget(widget: Widget, area: Rect): Unit
  def renderStatefulWidget[W <: StatefulWidget](widget: W, area: Rect)(state: widget.State): Unit
  def setCursor(x: Int, y: Int): Unit
