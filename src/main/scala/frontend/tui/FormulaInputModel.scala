package proofPlayground
package frontend.tui

enum InputMode:
  case Normal
  case Editing

object FormulaInputModel:
  trait Data:
    def mode: InputMode
    def formula: String

  trait Signals:
    def character(c: Char): Unit
    def backspace(): Unit
    def edit(): Unit
    def clear(): Unit
    def submit(): Unit
    def quit(): Unit

class FormulaInputModel(shouldExit: () => Unit) extends FormulaInputModel.Data, FormulaInputModel.Signals:
  private var formulaText: String  = ""
  private var inputMode: InputMode = InputMode.Normal

  override def mode: InputMode = inputMode
  override def formula: String = formulaText

  override def character(c: Char): Unit =
    formulaText += c

  override def backspace(): Unit =
    formulaText = formulaText.dropRight(1)

  override def edit(): Unit =
    inputMode = InputMode.Editing

  override def clear(): Unit =
    formulaText = ""
    inputMode = InputMode.Normal

  override def submit(): Unit = () // TODO

  override def quit(): Unit = shouldExit()
