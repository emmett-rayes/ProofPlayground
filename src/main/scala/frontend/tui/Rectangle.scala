package proofPlayground
package frontend.tui

import tui.{Constraint, Direction, Layout, Rect}

object Rectangle:
  def apply(ySize: Int, xSize: Int, area: Rect): Rect =
    val yLayout = Layout(
      direction = Direction.Vertical,
      constraints = Array(
        Constraint.Percentage((100 - ySize) / 2),
        Constraint.Percentage(ySize),
        Constraint.Percentage((100 - ySize) / 2)
      )
    ).split(area)

    val xLayout = Layout(
      direction = Direction.Horizontal,
      constraints = Array(
        Constraint.Percentage((100 - xSize) / 2),
        Constraint.Percentage(xSize),
        Constraint.Percentage((100 - xSize) / 2),
      )
    ).split(yLayout(1))

    xLayout(1)
