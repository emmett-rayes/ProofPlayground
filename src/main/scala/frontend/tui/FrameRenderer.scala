package proofPlayground
package frontend.tui

import tui.{Frame, Rect, StatefulWidget, Widget}

class FrameRenderer(frame: Frame) extends Renderer:
  override def render(widget: Widget, area: Rect): Unit =
    frame.renderWidget(widget, area)

  override def render[W <: StatefulWidget](widget: W, area: Rect)(state: widget.State): Unit =
    frame.renderStatefulWidget(widget, area)(state)

  override def setCursor(x: Int, y: Int): Unit =
    frame.setCursor(x, y)
