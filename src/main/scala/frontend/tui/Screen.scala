package proofPlayground
package frontend.tui

import tui.{Rect, Text}

trait Screen:
  def headerText: Text
  def footerText: Text
  def render(renderer: Renderer, area: Rect): Unit
  def handleEvent(event: tui.crossterm.Event): Unit
