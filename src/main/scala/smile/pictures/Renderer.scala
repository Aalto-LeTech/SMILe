package smile.pictures

import smile.infrastructure.Constants.MaximumOpacity
import smile.infrastructure.{BufferAdapter, DrawingSurface}
import smile.modeling.BoundaryCalculator

/** Provides functionality for rendering pictures and their elements into bitmaps.
  */
object Renderer:
  /** Creates a bitmap representation of a sequence of picture elements.
    *
    * @param elements
    *   A sequence of [[PictureElement]] instances to be rendered into a bitmap.
    * @return
    *   A [[Bitmap]] instance representing the rendered picture elements.
    */
  def createBitmapFrom(elements: PictureElement*): Bitmap = createBitmapFrom(new Picture(elements))

  /** Creates a bitmap from a given picture.
    *
    * @param picture
    *   The [[Picture]] to be rendered into a bitmap.
    * @return
    *   A [[Bitmap]] representing the rendered picture.
    */
  def createBitmapFrom(picture: Picture): Bitmap =
    if picture.elements.isEmpty then return new Bitmap(0, 0)

    val bounds = picture.viewport match
      case Some(viewport) => viewport.boundary
      case None           => BoundaryCalculator.fromBoundaries(picture.elements)

    val flooredWidth  = bounds.width.floor
    val flooredHeight = bounds.height.floor

    if flooredWidth < 1 || flooredHeight < 1 then return new Bitmap(0, 0)

    require(flooredWidth > 0 && flooredHeight > 0, "Bitmap width and height must be positive")

    val buffer = new BufferAdapter(flooredWidth, flooredHeight)

    val (xOffsetToOrigin, yOffsetToOrigin) =
      val upperLeftCorner = bounds.upperLeftCorner

      val xOffset = -upperLeftCorner.x
      val yOffset = -upperLeftCorner.y

      (xOffset, yOffset)

    renderElements(
      picture,
      new DrawingSurface(buffer),
      xOffsetToOrigin,
      yOffsetToOrigin
    )

    new Bitmap(buffer, bounds)
  end createBitmapFrom

  /** Renders the elements of a picture onto a target drawing surface.
    *
    * @param content
    *   The [[Picture]] containing elements to be rendered.
    * @param targetDrawingSurface
    *   The [[DrawingSurface]] where elements will be rendered.
    * @param xOffsetToOrigin
    *   The X offset to the origin point for rendering.
    * @param yOffsetToOrigin
    *   The Y offset to the origin point for rendering.
    */
  private def renderElements(
      content: Picture,
      targetDrawingSurface: DrawingSurface,
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double
  ): Unit =
    for element <- content.elements.reverse do
      renderElement(
        element,
        targetDrawingSurface,
        xOffsetToOrigin,
        yOffsetToOrigin
      )

  /** Renders an individual element onto a target drawing surface.
    *
    * @param contentItem
    *   The [[PictureElement]] to be rendered.
    * @param targetDrawingSurface
    *   The [[DrawingSurface]] where the element will be rendered.
    * @param xOffsetToOrigin
    *   The X offset to the origin point for rendering.
    * @param yOffsetToOrigin
    *   The Y offset to the origin point for rendering.
    */
  private def renderElement(
      contentItem: PictureElement,
      targetDrawingSurface: DrawingSurface,
      xOffsetToOrigin: Double,
      yOffsetToOrigin: Double
  ): Unit =
    contentItem match
      case arc: Arc =>
        targetDrawingSurface.drawArc(
          xOffsetToOrigin,
          yOffsetToOrigin,
          arc.position.x,
          arc.position.y,
          arc.width,
          arc.height,
          arc.startAngle,
          arc.arcAngle,
          arc.rotationAngle,
          arc.hasBorder,
          arc.hasFilling,
          arc.color,
          arc.fillColor
        )
      case bitmap: Bitmap =>
        val topLeft  = bitmap.boundary.upperLeftCorner
        val topLeftX = xOffsetToOrigin + topLeft.x
        val topLeftY = yOffsetToOrigin + topLeft.y

        targetDrawingSurface.drawBitmap(
          bitmap.buffer.get,
          topLeftX,
          topLeftY,
          MaximumOpacity
        )
      case point: Point =>
        targetDrawingSurface.drawPoint(
          xOffsetToOrigin,
          yOffsetToOrigin,
          point.position.x,
          point.position.y,
          point.color
        )
      case polygon: Polygon =>
        if polygon.points.nonEmpty then
          val position = polygon.position
          val points   = polygon.points

          val (xs, ys) = points.unzip(p => (p.x, p.y))

          targetDrawingSurface.drawPolygon(
            xOffsetToOrigin,
            yOffsetToOrigin,
            position.x,
            position.y,
            xs,
            ys,
            points.length,
            polygon.hasBorder,
            polygon.hasFilling,
            polygon.color,
            polygon.fillColor
          )
      case text: Text => targetDrawingSurface.drawText(text)

  end renderElement
