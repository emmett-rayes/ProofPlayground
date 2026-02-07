package proofPlayground
package frontend.tui.widgets

import tui.*

/** Ported using AI from https://github.com/ratatui/tui-widgets/tree/main/tui-scrollview
  *
  * A widget that can scroll its contents
  *
  * Allows you to render a widget into a buffer larger than the area it is rendered into, and then
  * scroll the contents of that buffer around.
  *
  * Note that the origin of the buffer is always at (0, 0), and the buffer is always the size of the
  * size passed to `new`. The `ScrollViewWidget` widget itself is responsible for rendering the visible
  * area of the buffer into the main buffer.
  *
  * @example
  * val size = Size(20, 20)
  * var scrollView = ScrollViewWidget(size)
  *
  * // render a few widgets into the buffer at various positions
  * scrollView.renderWidget(ParagraphWidget(text = Text.nostyle("Hello, world!")), Rect(0, 0, 20, 1))
  * scrollView.renderWidget(ParagraphWidget(text = Text.nostyle("Hello, world!")), Rect(10, 10, 20, 1))
  *
  * val state = ScrollViewState()
  *
  * // you can also scroll the view programmatically
  * state.scrollDown()
  *
  * // render the scroll view into the main buffer
  * val scrollViewArea = Rect(0, 0, 10, 10)
  * scrollView.render(scrollViewArea, buf, state)
  * }}}
  */
case class ScrollViewWidget(
  size: Size,
  verticalScrollbarVisibility: ScrollbarVisibility = ScrollbarVisibility.Automatic,
  horizontalScrollbarVisibility: ScrollbarVisibility = ScrollbarVisibility.Automatic
) extends Widget, StatefulWidget {

  override type State = ScrollViewState

  val area        = Rect(x = 0, y = 0, width = size.width, height = size.height)
  private val buf: Buffer = Buffer.empty(area)

  /** The content size of the scroll view */
  def contentSize: Size = size

  /** The area of the buffer that is available to be scrolled */
  def bufferArea: Rect = buf.area

  /** The buffer containing the contents of the scroll view */
  def buffer: Buffer = buf

  /** Set the visibility of the vertical scrollbar */
  def withVerticalScrollbarVisibility(visibility: ScrollbarVisibility): ScrollViewWidget =
    copy(verticalScrollbarVisibility = visibility)

  /** Set the visibility of the horizontal scrollbar */
  def withHorizontalScrollbarVisibility(visibility: ScrollbarVisibility): ScrollViewWidget =
    copy(horizontalScrollbarVisibility = visibility)

  /** Set the visibility of both vertical and horizontal scrollbars */
  def withScrollbarsVisibility(visibility: ScrollbarVisibility): ScrollViewWidget =
    copy(
      verticalScrollbarVisibility = visibility,
      horizontalScrollbarVisibility = visibility
    )

  /** Render a widget into the scroll buffer */
  def renderWidget(widget: Widget, area: Rect): Unit = {
    widget.render(area, buf)
  }

  /** Render a stateful widget into the scroll buffer */
  def renderStatefulWidget[W <: StatefulWidget](widget: W, area: Rect, state: widget.State): Unit = {
    widget.render(area, buf, state)
  }

  // Widget.render implementation (for non-stateful rendering)
  override def render(area: Rect, buf: Buffer): Unit = {
    val state = ScrollViewState()
    render(area, buf, state)
  }

  // StatefulWidget.render implementation
  override def render(area: Rect, buf: Buffer, state: ScrollViewState): Unit = {
    var x = state.offset.x
    var y = state.offset.y

    // ensure that we don't scroll past the end of the buffer in either direction
    val maxXOffset = this.buf.area.width - (area.width - 1).max(0)
    val maxYOffset = this.buf.area.height - (area.height - 1).max(0)

    x = x.min(maxXOffset).max(0)
    y = y.min(maxYOffset).max(0)
    state.offset = Position(x, y)
    state.size = Some(size)
    state.pageSize = Some(Size(area.width, area.height))

    val visibleArea = renderScrollbars(area, buf, state).intersection(this.buf.area)
    renderVisibleArea(area, buf, visibleArea)
  }

  /** Render needed scrollbars and return remaining area relative to scrollview's buffer area. */
  private def renderScrollbars(area: Rect, buf: Buffer, state: ScrollViewState): Rect = {
    // fit value per direction
    //   > 0 => fits
    //  == 0 => exact fit
    //   < 0 => does not fit
    val horizontalSpace = area.width - size.width
    val verticalSpace   = area.height - size.height

    // if it fits in that direction, reset state to reflect it
    if horizontalSpace > 0 then
      state.offset = state.offset.copy(x = 0)

    if verticalSpace > 0 then
      state.offset = state.offset.copy(y = 0)

    val (showHorizontal, showVertical) = visibleScrollbars(horizontalSpace, verticalSpace)

    val newHeight = if showHorizontal then
      // if both bars are rendered, avoid the corner
      val width      = area.width - (if showVertical then 1 else 0)
      val renderArea = area.copy(width = width)
      // render scrollbar, update available space
      renderHorizontalScrollbar(renderArea, buf, state)
      area.height - 1
    else
      area.height

    val newWidth = if showVertical then
      // if both bars are rendered, avoid the corner
      val height     = area.height - (if showHorizontal then 1 else 0)
      val renderArea = area.copy(height = height)
      // render scrollbar, update available space
      renderVerticalScrollbar(renderArea, buf, state)
      area.width - 1
    else
      area.width

    Rect(x = state.offset.x, y = state.offset.y, width = newWidth, height = newHeight)
  }

  /** Resolve whether to render each scrollbar. */
  private def visibleScrollbars(horizontalSpace: Int, verticalSpace: Int): (Boolean, Boolean) = {
    (horizontalScrollbarVisibility, verticalScrollbarVisibility) match {
      // straightforward, no need to check fit values
      case (ScrollbarVisibility.Always, ScrollbarVisibility.Always) => (true, true)
      case (ScrollbarVisibility.Never, ScrollbarVisibility.Never)   => (false, false)
      case (ScrollbarVisibility.Always, ScrollbarVisibility.Never)  => (true, false)
      case (ScrollbarVisibility.Never, ScrollbarVisibility.Always)  => (false, true)

      // Auto => render scrollbar only if it doesn't fit
      case (ScrollbarVisibility.Automatic, ScrollbarVisibility.Never) => (horizontalSpace < 0, false)
      case (ScrollbarVisibility.Never, ScrollbarVisibility.Automatic) => (false, verticalSpace < 0)

      // Auto => render scrollbar if it doesn't fit or exact fit
      case (ScrollbarVisibility.Always, ScrollbarVisibility.Automatic) => (true, verticalSpace <= 0)
      case (ScrollbarVisibility.Automatic, ScrollbarVisibility.Always) => (horizontalSpace <= 0, true)

      // depends solely on fit values
      case (ScrollbarVisibility.Automatic, ScrollbarVisibility.Automatic) =>
        if horizontalSpace >= 0 && verticalSpace >= 0 then
          (false, false)
        else if horizontalSpace < 0 && verticalSpace < 0 then
          (true, true)
        else if horizontalSpace > 0 && verticalSpace < 0 then
          (false, true)
        else if horizontalSpace < 0 && verticalSpace > 0 then
          (true, false)
        else
          // one is an exact fit and other does not fit
          (true, true)
    }
  }

  private def renderVerticalScrollbar(area: Rect, buf: Buffer, state: ScrollViewState): Unit = {
    val scrollbarHeight = size.height - area.height
    val scrollbar       = ScrollbarWidget(
      orientation = ScrollbarOrientation.VerticalRight,
      contentLength = scrollbarHeight.max(0),
      position = state.offset.y.max(0)
    )
    scrollbar.render(area, buf)
  }

  private def renderHorizontalScrollbar(area: Rect, buf: Buffer, state: ScrollViewState): Unit = {
    val scrollbarWidth = size.width - area.width
    val scrollbar      = ScrollbarWidget(
      orientation = ScrollbarOrientation.HorizontalBottom,
      contentLength = scrollbarWidth.max(0),
      position = state.offset.x.max(0)
    )
    scrollbar.render(area, buf)
  }

  private def renderVisibleArea(area: Rect, buf: Buffer, visibleArea: Rect): Unit = {
    // Copy cells from scroll buffer to main buffer
    var srcY = visibleArea.top
    var dstY = area.top
    while srcY < visibleArea.bottom && dstY < area.bottom do
      var srcX = visibleArea.left
      var dstX = area.left
      while srcX < visibleArea.right && dstX < area.right do
        val srcCell = this.buf.get(srcX, srcY)
        buf.set(dstX, dstY, srcCell.clone())
        srcX += 1
        dstX += 1
      srcY += 1
      dstY += 1
  }

}

/** The visibility of the vertical and horizontal scrollbars. */
sealed trait ScrollbarVisibility

object ScrollbarVisibility {

  /** Render the scrollbar only whenever needed. */
  case object Automatic extends ScrollbarVisibility

  /** Always render the scrollbar. */
  case object Always extends ScrollbarVisibility

  /** Never render the scrollbar (hide it). */
  case object Never extends ScrollbarVisibility
}

/** A simple case class representing a 2D size */
case class Size(width: Int, height: Int)

/** A simple case class representing a 2D position */
case class Position(x: Int, y: Int)

object Position {
  val ORIGIN: Position = Position(0, 0)
}

/** State for the ScrollView widget */
case class ScrollViewState(
  var offset: Position = Position.ORIGIN,
  var size: Option[Size] = None,
  var pageSize: Option[Size] = None
) {

  /** Set the offset of the scroll view state */
  def setOffset(newOffset: Position): Unit = {
    offset = newOffset
  }

  /** Move the scroll view state up by one row */
  def scrollUp(): Unit = {
    offset = offset.copy(y = (offset.y - 1).max(0))
  }

  /** Move the scroll view state down by one row */
  def scrollDown(): Unit = {
    offset = offset.copy(y = offset.y + 1)
  }

  /** Move the scroll view state down by one page */
  def scrollPageDown(): Unit = {
    val pageSizeHeight = pageSize.map(_.height).getOrElse(1)
    offset = offset.copy(y = (offset.y + pageSizeHeight - 1).max(0))
  }

  /** Move the scroll view state up by one page */
  def scrollPageUp(): Unit = {
    val pageSizeHeight = pageSize.map(_.height).getOrElse(1)
    offset = offset.copy(y = ((offset.y + 1) - pageSizeHeight).max(0))
  }

  /** Move the scroll view state left by one column */
  def scrollLeft(): Unit = {
    offset = offset.copy(x = (offset.x - 1).max(0))
  }

  /** Move the scroll view state right by one column */
  def scrollRight(): Unit = {
    offset = offset.copy(x = offset.x + 1)
  }

  /** Move the scroll view state to the top of the buffer */
  def scrollToTop(): Unit = {
    offset = Position.ORIGIN
  }

  /** Move the scroll view state to the bottom of the buffer */
  def scrollToBottom(): Unit = {
    val bottom = size.map(s => (s.height - 1).max(0)).getOrElse(Int.MaxValue)
    offset = offset.copy(y = bottom)
  }

}

object ScrollViewState {
  def apply(): ScrollViewState = new ScrollViewState()

  def withOffset(offset: Position): ScrollViewState =
    new ScrollViewState(offset = offset)

}

/** Scrollbar orientation */
sealed trait ScrollbarOrientation

object ScrollbarOrientation {
  case object VerticalRight    extends ScrollbarOrientation
  case object VerticalLeft     extends ScrollbarOrientation
  case object HorizontalBottom extends ScrollbarOrientation
  case object HorizontalTop    extends ScrollbarOrientation
}

/** A simple scrollbar widget */
case class ScrollbarWidget(
  orientation: ScrollbarOrientation,
  contentLength: Int,
  position: Int,
  style: Style = Style.DEFAULT
) extends Widget {

  override def render(area: Rect, buf: Buffer): Unit = {
    orientation match {
      case ScrollbarOrientation.VerticalRight =>
        renderVertical(area, buf, isRight = true)
      case ScrollbarOrientation.VerticalLeft =>
        renderVertical(area, buf, isRight = false)
      case ScrollbarOrientation.HorizontalBottom =>
        renderHorizontal(area, buf, isBottom = true)
      case ScrollbarOrientation.HorizontalTop =>
        renderHorizontal(area, buf, isBottom = false)
    }
  }

  private def renderVertical(area: Rect, buf: Buffer, isRight: Boolean): Unit = {
    if area.height == 0 || contentLength == 0 then return

    val x           = if isRight then area.right - 1 else area.left
    val trackHeight = area.height

    // Calculate thumb size and position
    val thumbSize = math.max(1, (trackHeight * trackHeight) / (trackHeight + contentLength))
    val thumbPos  = if contentLength > 0 then
      ((position.toDouble / contentLength) * (trackHeight - thumbSize)).toInt
    else 0

    // Render track
    var y = area.top
    while y < area.bottom do
      val symbol = if y == area.top then
        "▲"
      else if y == area.bottom - 1 then
        "▼"
      else if y >= area.top + thumbPos + 1 && y < area.top + thumbPos + thumbSize + 1 then
        "█"
      else
        "║"
      buf.get(x, y).setSymbol(symbol).setStyle(style)
      y += 1
  }

  private def renderHorizontal(area: Rect, buf: Buffer, isBottom: Boolean): Unit = {
    if area.width == 0 || contentLength == 0 then return

    val y          = if isBottom then area.bottom - 1 else area.top
    val trackWidth = area.width

    // Calculate thumb size and position
    val thumbSize = math.max(1, (trackWidth * trackWidth) / (trackWidth + contentLength))
    val thumbPos  = if contentLength > 0 then
      ((position.toDouble / contentLength) * (trackWidth - thumbSize)).toInt
    else 0

    // Render track
    var x = area.left
    while x < area.right do
      val symbol = if x == area.left then
        "◄"
      else if x == area.right - 1 then
        "►"
      else if x >= area.left + thumbPos + 1 && x < area.left + thumbPos + thumbSize + 1 then
        "█"
      else
        "═"
      buf.get(x, y).setSymbol(symbol).setStyle(style)
      x += 1
  }

}
