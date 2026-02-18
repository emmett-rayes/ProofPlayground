package proofPlayground
package frontend.tui

import frontend.tui.Screen.EventResult

import tui.{Rect, Text}

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
