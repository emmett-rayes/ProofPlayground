package proofPlayground
package frontend.tui

import tui.Frame

trait Screen:
  def render(frame: Frame): Unit
  def handleEvent(event: tui.crossterm.Event): Unit
