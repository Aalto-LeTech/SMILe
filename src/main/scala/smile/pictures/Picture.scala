package smile.pictures

import smile.colors.Color
import smile.modeling.*

/** Represents a picture composed of various graphical elements and an optional viewport.
  *
  * @param elements
  *   A sequence of `PictureElement` instances that make up the picture.
  * @param viewport
  *   An optional viewport that defines the visible area of the picture.
  */
class Picture(
    val elements: Seq[PictureElement],
    val viewport: Option[Viewport]
) extends Transformable[Picture]:

  /** A map of reference points defined within the picture, keyed by their names. */
  val referencePoints: Map[String, Pos] = elements
    .collect:
      case e: ReferencePoint => e.name -> e.position
    .toMap

  /** Creates a `Picture` with a single graphical element and no predefined viewport.
    *
    * @param element
    *   The `PictureElement` to include in the picture.
    */
  def this(element: PictureElement) =
    this(Seq(element), None)

  /** Creates a `Picture` with a sequence of graphical elements and no predefined viewport.
    *
    * @param elements
    *   A sequence of `PictureElement` instances to include in the picture.
    */
  def this(elements: Seq[PictureElement]) = this(elements, None)

  /** Creates an empty `Picture` with no graphical elements and no predefined viewport.
    */
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

  override lazy val boundary: Bounds = BoundaryCalculator.fromBoundaries(elements)

  def setViewport(viewport: Viewport): Picture =
    copy(newViewport = Option(viewport))

  def withContentBoundaryAsViewport: Picture =
    copy(newViewport = Option(Viewport(boundary)))

  def removeViewport(): Picture = copy(newViewport = None)

  /** Merges the pixels of this picture with another picture using a specified pixel merging
    * function.
    *
    * @param another
    *   The picture to merge with.
    * @param pixelMerger
    *   The function to merge pixels of overlapping elements.
    * @return
    *   A `Bitmap` resulting from the pixel merge.
    */
  def mergePixelsWith(another: Picture, pixelMerger: (Color, Color) => Color): Bitmap =
    toBitmap.mergeWith(another.toBitmap, pixelMerger)

  /** Applies a transformation function to each element in the picture.
    *
    * @param f
    *   The transformation function to apply to each `PictureElement`.
    * @return
    *   A new `Picture` instance with the transformed elements.
    */
  def map(f: PictureElement => PictureElement): Picture =
    copy(newElements = elements.map(f))

  override def scaleBy(horizontalFactor: Double, verticalFactor: Double): Picture =
    val relativityPoint = this.boundary.center

    val scaledPos = this.position.scaleBy(horizontalFactor, verticalFactor, relativityPoint)
    val returnToPositionOffset = this.position - scaledPos

    map(
      _.scaleBy(horizontalFactor, verticalFactor, relativityPoint)
        .moveBy(returnToPositionOffset.x, returnToPositionOffset.y)
    )
  end scaleBy

  override def scaleBy(
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

  override inline def rotateByAroundOrigin(angle: Double): Picture =
    map(_.rotateByAroundOrigin(angle))
