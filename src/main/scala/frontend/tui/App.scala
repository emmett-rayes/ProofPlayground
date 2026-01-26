package proofPlayground
package frontend.tui

import tui.withTerminal

@main
def main(): Unit =
  val coordinator = Coordinator()
  withTerminal { (jni, terminal) =>
    while !coordinator.shouldExit do
      terminal.draw(coordinator.render)
      coordinator.handleEvent(jni.read())
  }
