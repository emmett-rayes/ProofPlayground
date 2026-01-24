package proofPlayground
package frontend.tui

enum InputMode:
  case Normal
  case Editing

object FormulaInputModel:
  trait Data:
    def mode: InputMode
    def formula: String
    def cursor: Int

  trait Signals:
    def character(c: Char): Unit
    def backspace(): Unit
    def cursorLeft(): Unit
    def cursorRight(): Unit
    def edit(): Unit
    def clear(): Unit
    def submit(): Unit
    def quit(): Unit

class FormulaInputModel(shouldExit: () => Unit) extends FormulaInputModel.Data, FormulaInputModel.Signals:
  private var formulaText: String  = ""
  private var inputMode: InputMode = InputMode.Normal
  private var cursorPosition: Int = 0

  override def mode: InputMode = inputMode
  override def formula: String = formulaText
  override def cursor: Int = cursorPosition

  override def character(c: Char): Unit =
    if inputMode == InputMode.Editing then
      formulaText = formulaText.patch(cursorPosition, c.toString, 0)
      cursorPosition += 1

  override def backspace(): Unit =
    if inputMode == InputMode.Editing && cursorPosition > 0 && formulaText.nonEmpty then
      formulaText = formulaText.patch(cursorPosition - 1, "", 1)
      cursorPosition -= 1

  override def edit(): Unit =
    inputMode = InputMode.Editing
    cursorPosition = formulaText.length

  override def clear(): Unit =
    formulaText = ""
    inputMode = InputMode.Normal
    cursorPosition = 0

  override def submit(): Unit = () // TODO

  override def quit(): Unit = shouldExit()

  override def cursorLeft(): Unit =
    if inputMode == InputMode.Editing && cursorPosition > 0 then
      cursorPosition -= 1

  override def cursorRight(): Unit =
    if inputMode == InputMode.Editing && cursorPosition < formulaText.length then
      cursorPosition += 1
