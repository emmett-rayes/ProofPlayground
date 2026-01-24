package proofPlayground
package frontend.tui

import core.logic.propositional.Formula
import frontend.notation.logic.FormulaParser.parser

import scala.util.Success

enum InputMode:
  case Normal
  case Editing
  case Error(message: String)

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
    def back(): Unit
    def clear(): Unit
    def submit(): Unit
    def quit(): Unit

class FormulaInputModel(shouldExit: () => Unit) extends FormulaInputModel.Data, FormulaInputModel.Signals:
  private var formulaText: String  = ""
  private var inputMode: InputMode = InputMode.Normal
  private var cursorPosition: Int  = 0

  override def mode: InputMode = inputMode
  override def formula: String = formulaText
  override def cursor: Int     = cursorPosition

  override def character(c: Char): Unit =
    if inputMode == InputMode.Editing then
      formulaText = formulaText.patch(cursorPosition, c.toString, 0)
      cursorPosition += 1

  override def backspace(): Unit =
    if inputMode == InputMode.Editing then
      formulaText = formulaText.patch(cursorPosition - 1, "", 1)
      cursorPosition -= 1

  override def edit(): Unit =
    inputMode = InputMode.Editing
    cursorPosition = formulaText.length

  override def back(): Unit =
    inputMode match
      case InputMode.Normal   => ()
      case InputMode.Error(_) => inputMode = InputMode.Editing
      case InputMode.Editing  => inputMode = InputMode.Normal

  override def clear(): Unit =
    if inputMode == InputMode.Normal then
      formulaText = ""
      cursorPosition = 0

  override def submit(): Unit =
    Formula.parser.parse(formulaText) match
      case Success(value) if value.remaining.isEmpty =>
        inputMode = InputMode.Normal
      case _ =>
        inputMode = InputMode.Error("Invalid formula.")

  override def quit(): Unit = shouldExit()

  override def cursorLeft(): Unit =
    if inputMode == InputMode.Editing && cursorPosition > 0 then
      cursorPosition -= 1

  override def cursorRight(): Unit =
    if inputMode == InputMode.Editing && cursorPosition < formulaText.length then
      cursorPosition += 1
