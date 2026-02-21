package proofPlayground
package frontend.tui

import tui.{Rect, Text}

import frontend.tui.Screen.EventResult

object Screen {
  enum EventResult {
    case Handled
    case NotHandled
  }
}

trait Screen {
  def handleEvent(event: tui.crossterm.Event): EventResult
  def headerText: Text
  def footerText: Text
  def render(renderer: Renderer, area: Rect): Unit
}
