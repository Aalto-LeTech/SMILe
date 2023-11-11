package smile.pictures

import smile.colors.Color
import smile.modeling.*

class Picture(val elements: Seq[PictureElement], val viewport: Option[Viewport])
    extends Transformable[Picture]:

  def this(element: PictureElement) =
    this(Seq(element), None)

  def this(elements: Seq[PictureElement]) = this(elements, None)

  def this() = this(Seq(), None)

  override lazy val position: Pos = boundary.center
  lazy val width: Len             = boundary.width
  lazy val height: Len            = boundary.height

  override def moveBy(xOffset: Double, yOffset: Double): Picture =
    map(_.moveBy(xOffset, yOffset))

  override def copy(newPosition: Pos): Picture =
//    Picture(elements.map(_.moveTo(newPosition.x, newPosition.y, PositionType.Center)), viewport)
    moveTo(newPosition.x, newPosition.y, PositionType.Center) // TODO: check

  def copy(newViewport: Option[Viewport]): Picture =
    Picture(elements, newViewport)

  def copy(newElements: Seq[PictureElement]): Picture =
    Picture(newElements, viewport)

  lazy val hasViewport: Boolean = viewport.isDefined

  lazy val boundary: Bounds = BoundaryCalculator.fromBoundaries(elements)

  def setViewport(viewport: Viewport): Picture =
    copy(newViewport = Option(viewport))

  def withContentBoundaryAsViewport: Picture =
    copy(newViewport = Option(Viewport(boundary)))

  def removeViewport(): Picture = copy(newViewport = None)

  /** @param another
    * @param pixelMerger
    * @return
    */
  def mergePixelsWith(another: Picture, pixelMerger: (Color, Color) => Color): Bitmap =
    toBitmap.mergeWith(another.toBitmap, pixelMerger)

  def map(f: (PictureElement) => PictureElement): Picture =
    copy(newElements = elements.map(f))

  def scaleBy(horizontalFactor: Double, verticalFactor: Double): Picture =
    val relativityPoint = this.boundary.center

    val scaledPos = this.position.scaleBy(horizontalFactor, verticalFactor, relativityPoint)
    val returnToPositionOffset = this.position - scaledPos

    map(
      _.scaleBy(horizontalFactor, verticalFactor, relativityPoint)
        .moveBy(returnToPositionOffset.x, returnToPositionOffset.y)
    )
  end scaleBy

  /** Scales this object by given horizontal and vertical factors in relation to a given point.
    *
    * @param horizontalFactor
    * @param verticalFactor
    * @param relativityPoint
    * @return
    */
  def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): Picture =
    val scaledPic = scaleBy(horizontalFactor, verticalFactor)

    val relativityDistance = scaledPic.position - relativityPoint
    val scaledXOffset =
      horizontalFactor * relativityDistance.x - relativityDistance.x
    val scaledYOffset = verticalFactor * relativityDistance.y - relativityDistance.y

    scaledPic.map(_.moveBy(scaledXOffset, scaledYOffset))
  end scaleBy

  override inline def rotateBy(angle: Double, centerOfRotation: Pos): Picture =
    map(_.rotateBy(angle, centerOfRotation))

  override inline def rotateByAroundOrigo(angle: Double): Picture =
    map(_.rotateByAroundOrigo(angle))

  def toAsciiArt: String = toBitmap.toAsciiArt()

  def toAsciiColorBlocks: String = toBitmap.toAsciiColorBlocks()
