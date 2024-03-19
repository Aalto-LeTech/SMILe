package smile.pictures

import smile.Settings.*
import smile.modeling.*

import scala.annotation.tailrec

object Transformable:

  /** A type alias for a function that transforms a picture into another picture. */
  type SimpleTransformer = Picture => Picture

  /** A simple transformer that returns the input picture without any modifications. */
  val IdentitySimpleTransformer: SimpleTransformer = (i: Picture) => i

/** A trait for graphical elements that can undergo transformations like translation, scaling, and
  * rotation.
  * @tparam TransformableType
  *   The type of the transformable object.
  */
trait Transformable[TransformableType <: Transformable[TransformableType]]:

  /** The bounding box of the transformable object. */
  lazy val boundary: Bounds

  /** The center position of the transformable object. */
  lazy val position: Pos = boundary.center

  /** Creates a copy of this transformable object positioned at the specified coordinates.
    *
    * @param newPosition
    *   The new position for the copied object.
    * @return
    *   A new instance of the transformable object at the specified position.
    */
  def copy(newPosition: Pos): TransformableType

  /** Converts the transformable object to a bitmap representation.
    *
    * @return
    *   A bitmap representation of the object.
    */
  def toBitmap: Bitmap = this match
    case picture: Picture =>
      if picture.elements.length != 1 then Bitmap(picture)
      else
        picture.elements.head match
          case bitmap: Bitmap =>
            bitmap
          case element: PictureElement =>
            Bitmap(element)
    case element: PictureElement =>
      Bitmap(element)

  private inline def asPicture: Picture = this match
    case picture: Picture =>
      picture
    case element: PictureElement =>
      new Picture(element)

  /** Represents an element with its coordinates, used for positioning elements at specific
    * locations.
    *
    * @param element
    *   The picture element to be positioned.
    * @param x
    *   The x-coordinate in pixels for positioning.
    * @param y
    *   The y-coordinate in pixels for positioning.
    */
  case class ElementWithCoordinates(element: PictureElement, x: Double, y: Double)

  /** Adds another transformable object at specified coordinates and position type.
    *
    * @param contentsAndCoordinates
    *   A sequence of elements with their corresponding positions.
    * @param positionType
    *   The type of position reference (e.g., center or upper left corner).
    * @return
    *   A picture with the added content.
    */
  def addAt(
      contentsAndCoordinates: Seq[ElementWithCoordinates],
      positionType: PositionType
  ): Picture =
    val movedContent = contentsAndCoordinates.map:
      case ElementWithCoordinates(element, x, y) =>
        element.moveTo(x, y, positionType)

    addToFront(new Picture(movedContent))

  /** Adds another transformable object at a specified position.
    *
    * @param contentAndCoordinates
    *   The element and its position.
    * @param positionType
    *   The type of position reference.
    * @return
    *   A picture with the added content.
    */
  def addAt(
      contentAndCoordinates: ElementWithCoordinates,
      positionType: PositionType
  ): Picture =
    addAt(
      contentAndCoordinates.element,
      contentAndCoordinates.x,
      contentAndCoordinates.y,
      positionType
    )

  /** Adds another transformable object at a specified position.
    *
    * @param content
    *   The content to add.
    * @param x
    *   The x-coordinate in pixels for the new position.
    * @param y
    *   The y-coordinate in pixels for the new position.
    * @param positionType
    *   The type of position reference.
    * @return
    *   A picture with the added content.
    */
  def addAt(
      content: Transformable[?],
      x: Double,
      y: Double,
      positionType: PositionType = DefaultPositionType
  ): Picture =
    addToFront(
      content.moveTo(x, y, positionType)
    )

  /** Adds the given transformable object to the front of this object.
    *
    * @param toPrepend
    *   The object to add to the front.
    * @return
    *   A picture with the object added to the front.
    */
  def addToFront(toPrepend: Transformable[?]): Picture =
    prependTo(toPrepend.asPicture, this.asPicture)

  /** Adds the given transformable object to the back of this object.
    *
    * @param toAppend
    *   The object to add to the back.
    * @return
    *   A picture with the object added to the back.
    */
  def addToBack(toAppend: Transformable[?]): Picture =
    appendTo(toAppend.asPicture, this.asPicture)

  protected final def appendTo(
      contentToAppend: Picture,
      existingContent: Picture
  ): Picture =
    new Picture(existingContent.elements ++ contentToAppend.elements)

  protected final def prependTo(
      contentToPrepend: Picture,
      existingContent: Picture
  ): Picture =
    new Picture(contentToPrepend.elements ++ existingContent.elements)

  /** Adds content to a specified side of this object, optionally with padding.
    *
    * @param targetSide
    *   The side to add content to (Top, Bottom, Left, Right).
    * @param content
    *   The content to add.
    * @param padding
    *   The padding in pixels between the existing content and the added content.
    * @param alignment
    *   The alignment of the added content relative to the target side.
    * @return
    *   A picture with the content added to the specified side.
    */
  def addTo(
      targetSide: Side,
      content: Transformable[?],
      padding: Double = DefaultPaddingInPixels,
      alignment: SideIndependentAlignment
  ): Picture =
    targetSide match
      case Side.Top    => addToTop(content, padding, alignment.toHorizontal)
      case Side.Bottom => addToBottom(content, padding, alignment.toHorizontal)
      case Side.Left   => addToLeft(content, padding, alignment.toVertical)
      case Side.Right  => addToRight(content, padding, alignment.toVertical)

  /** Adds content above this object, optionally with padding and alignment.
    * @param content
    *   The content to add above this object.
    * @param padding
    *   The padding in pixels above this object.
    * @param alignment
    *   The horizontal alignment of the added content.
    * @return
    *   A picture with the content added above.
    */
  def addToTop(
      content: Transformable[?],
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment
  ): Picture =
    val newUpperLeftCornerX = boundary.horizontalPositionFor(alignment, content.boundary)

    val newUpperLeftCornerY =
      boundary.upperLeftCorner.y - padding - content.boundary.height.inPixels

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY, PositionType.UpperLeftCorner)
  end addToTop

  /** Adds content to the right of this object, optionally with padding and vertical alignment.
    * @param content
    *   The content to add to the right.
    * @param padding
    *   The padding in pixels to the right of this object.
    * @param alignment
    *   The vertical alignment of the added content.
    * @return
    *   A picture with the content added to the right.
    */
  def addToRight(
      content: Transformable[?],
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment
  ): Picture =
    val newUpperLeftCornerX = boundary.lowerRightCorner.x + padding
    val newUpperLeftCornerY = boundary.verticalPositionFor(alignment, content.boundary)

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY, PositionType.UpperLeftCorner)
  end addToRight

  /** Adds content below this object, optionally with padding and horizontal alignment.
    * @param content
    *   The content to add below this object.
    * @param padding
    *   The padding in pixels below this object.
    * @param alignment
    *   The horizontal alignment of the added content.
    * @return
    *   A picture with the content added below.
    */
  def addToBottom(
      content: Transformable[?],
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment
  ): Picture =
    val newUpperLeftCornerX = boundary.horizontalPositionFor(alignment, content.boundary)
    val newUpperLeftCornerY = boundary.lowerRightCorner.y + padding

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY, PositionType.UpperLeftCorner)
  end addToBottom

  /** Adds content to the left of this object, optionally with padding and vertical alignment.
    * @param content
    *   The content to add to the left.
    * @param padding
    *   The padding in pixels to the left of this object.
    * @param alignment
    *   The vertical alignment of the added content.
    * @return
    *   A picture with the content added to the left.
    */
  def addToLeft(
      content: Transformable[?],
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment
  ): Picture =
    val newUpperLeftCornerX =
      boundary.upperLeftCorner.x - padding - content.boundary.width.inPixels

    val newUpperLeftCornerY = boundary.verticalPositionFor(alignment, content.boundary)

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY, PositionType.UpperLeftCorner)
  end addToLeft

  // Alternating
  // -------------------------------------------------------------------------------------------- \\

  /** Alternates adding transformable objects upwards from this object.
    *
    * @param alternatives
    *   A sequence of transformable objects to alternate with.
    * @param numberOfAlternations
    *   The number of times to alternate additions.
    * @param padding
    *   The vertical space between each added object.
    * @param alignment
    *   The horizontal alignment for each added object.
    * @return
    *   A picture with the alternated additions applied.
    * @see
    *   [[alternateWith]]
    */
  def alternateUpwardsWith(
      alternatives: Seq[Transformable[?]],
      numberOfAlternations: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment
  ): Picture =
    alternateWith(alternatives, Side.Top, numberOfAlternations, padding, alignment.sideIndependent)

  /** Alternates adding transformable objects downwards from this object.
    *
    * @param alternatives
    *   A sequence of transformable objects to alternate with.
    * @param numberOfAlternations
    *   The number of times to alternate additions.
    * @param padding
    *   The vertical space between each added object.
    * @param alignment
    *   The horizontal alignment for each added object.
    * @return
    *   A picture with the alternated additions applied.
    * @see
    *   [[alternateWith]]
    */
  def alternateDownwardsWith(
      alternatives: Seq[Transformable[?]],
      numberOfAlternations: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment
  ): Picture =
    alternateWith(
      alternatives,
      Side.Bottom,
      numberOfAlternations,
      padding,
      alignment.sideIndependent
    )

  /** Alternates adding transformable objects leftwards from this object.
    *
    * @param alternatives
    *   A sequence of transformable objects to alternate with.
    * @param numberOfAlternations
    *   The number of times to alternate additions.
    * @param padding
    *   The horizontal space between each added object.
    * @param alignment
    *   The vertical alignment for each added object.
    * @return
    *   A picture with the alternated additions applied.
    * @see
    *   [[alternateWith]]
    */
  def alternateLeftwardsWith(
      alternatives: Seq[Transformable[?]],
      numberOfAlternations: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment
  ): Picture =
    alternateWith(alternatives, Side.Left, numberOfAlternations, padding, alignment.sideIndependent)

  /** Alternates adding transformable objects rightwards from this object.
    *
    * @param alternatives
    *   A sequence of transformable objects to alternate with.
    * @param numberOfAlternations
    *   The number of times to alternate additions.
    * @param padding
    *   The horizontal space between each added object.
    * @param alignment
    *   The vertical alignment for each added object.
    * @return
    *   A picture with the alternated additions applied.
    * @see
    *   [[alternateWith]]
    */
  def alternateRightwardsWith(
      alternatives: Seq[Transformable[?]],
      numberOfAlternations: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment
  ): Picture =
    alternateWith(
      alternatives,
      Side.Right,
      numberOfAlternations,
      padding,
      alignment.sideIndependent
    )

  /** Alternates adding transformable objects around this object on a specified side, for a given
    * number of alternations. This method allows for creating patterns or sequences of objects
    * around the original object, adding them one after another in a specified direction.
    *
    * @param alternatives
    *   A sequence of `Transformable` objects to alternate with. These objects are added in sequence
    *   around the original object.
    * @param side
    *   The side relative to this object where the alternations will be added (Top, Bottom, Left, or
    *   Right).
    * @param numberOfAlternations
    *   The total number of alternations to perform. This determines how many times objects from the
    *   `alternatives` sequence are added around this object.
    * @param padding
    *   The padding in pixels between each added object, with a default value defined by
    *   [[smile.Settings.DefaultPaddingInPixels]]. This spacing is applied between successive
    *   alternations.
    * @param alignment
    *   The alignment (either horizontal or vertical, depending on the side) of the added objects
    *   relative to the original object. This parameter controls how the added objects are aligned
    *   with respect to each other.
    * @return
    *   A `Picture` instance that includes the original object and the alternated additions.
    */
  def alternateWith(
      alternatives: Seq[Transformable[?]],
      side: Side,
      numberOfAlternations: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: SideIndependentAlignment
  ): Picture =

    val NumberOfFirstAlternation = 1

    require(
      numberOfAlternations >= 0,
      s"Number of alternations cannot be negative (was $numberOfAlternations)"
    )

    val numberOfSourceElements = alternatives.length + 1

    @tailrec
    def alternate(numberOfNextAlternation: Int, resultPicture: Picture): Picture =

      if numberOfNextAlternation > numberOfAlternations then return resultPicture

      val numberOfNextElement = numberOfNextAlternation % numberOfSourceElements
      val selectedElement =
        if numberOfNextElement == 0 then this
        else alternatives(numberOfNextElement - 1)

      alternate(
        numberOfNextAlternation + 1,
        resultPicture.addTo(side, selectedElement, padding, alignment)
      )
    end alternate

    alternate(NumberOfFirstAlternation, resultPicture = this.asPicture)

  end alternateWith

  // Replicating
  // -------------------------------------------------------------------------------------------- \\

  /** Replicates this object upwards a specified number of times with padding and alignment.
    *
    * @param numberOfReplicas
    *   The number of times to replicate this object.
    * @param padding
    *   The vertical space between each replica.
    * @param alignment
    *   The horizontal alignment for each replica.
    * @param transformer
    *   A function to apply a transformation to each replica.
    * @return
    *   A picture with the replicated additions applied.
    * @see
    *   [[replicateTo]]
    */
  def replicateUpwards(
      numberOfReplicas: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment,
      transformer: Transformable.SimpleTransformer = Transformable.IdentitySimpleTransformer
  ): Picture =
    replicateTo(Side.Top, numberOfReplicas, padding, alignment.sideIndependent, transformer)

  /** Replicates this object downwards a specified number of times with padding and alignment.
    *
    * @param numberOfReplicas
    *   The number of times to replicate this object.
    * @param padding
    *   The vertical space between each replica.
    * @param alignment
    *   The horizontal alignment for each replica.
    * @param transformer
    *   A function to apply a transformation to each replica.
    * @return
    *   A picture with the replicated additions applied.
    * @see
    *   [[replicateTo]]
    */
  def replicateDownwards(
      numberOfReplicas: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment,
      transformer: Transformable.SimpleTransformer = Transformable.IdentitySimpleTransformer
  ): Picture =
    replicateTo(
      Side.Bottom,
      numberOfReplicas,
      padding,
      alignment.sideIndependent,
      transformer
    )

  /** Replicates this object leftwards a specified number of times with padding and alignment.
    *
    * @param numberOfReplicas
    *   The number of times to replicate this object.
    * @param padding
    *   The vertical space between each replica.
    * @param alignment
    *   The horizontal alignment for each replica.
    * @param transformer
    *   A function to apply a transformation to each replica.
    * @return
    *   A picture with the replicated additions applied.
    * @see
    *   [[replicateTo]]
    */
  def replicateLeftwards(
      numberOfReplicas: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment,
      transformer: Transformable.SimpleTransformer = Transformable.IdentitySimpleTransformer
  ): Picture =
    replicateTo(
      Side.Left,
      numberOfReplicas,
      padding,
      alignment.sideIndependent,
      transformer
    )

  /** Replicates this object rightwards a specified number of times with padding and alignment.
    *
    * @param numberOfReplicas
    *   The number of times to replicate this object.
    * @param padding
    *   The vertical space between each replica.
    * @param alignment
    *   The horizontal alignment for each replica.
    * @param transformer
    *   A function to apply a transformation to each replica.
    * @return
    *   A picture with the replicated additions applied.
    * @see
    *   [[replicateTo]]
    */
  def replicateRightwards(
      numberOfReplicas: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment,
      transformer: Transformable.SimpleTransformer = Transformable.IdentitySimpleTransformer
  ): Picture =
    replicateTo(
      Side.Right,
      numberOfReplicas,
      padding,
      alignment.sideIndependent,
      transformer
    )

  /** Replicates this object to a specified direction a given number of times, each time potentially
    * transforming it and spacing the replicas with specified padding. This method allows for
    * creating sequences or arrays of similar objects with modifications.
    *
    * @param side
    *   The side (Top, Bottom, Left, Right) relative to this object where the replicas will be
    *   placed.
    * @param numberOfReplicas
    *   The total number of replicas to be created and placed. This number includes the original
    *   object in the count.
    * @param padding
    *   The space in pixels between each replica. This padding is applied uniformly between replicas
    *   to maintain consistent spacing.
    * @param alignment
    *   Determines how replicas are aligned relative to each other. This alignment can be horizontal
    *   or vertical, depending on the side specified for replication.
    * @param transformer
    *   A transformation function applied to each replica before it is placed. This allows for
    *   dynamic alterations of the replicas, such as scaling, rotation, or color changes, providing
    *   versatility in the visual outcomes.
    * @return
    *   A `Picture` that represents the combined visual result of all replicas, including the
    *   original object and its transformed replicas, arranged in the specified direction with the
    *   specified alignment and spacing.
    */
  def replicateTo(
      side: Side,
      numberOfReplicas: Int,
      padding: Double = DefaultPaddingInPixels,
      alignment: SideIndependentAlignment,
      transformer: Transformable.SimpleTransformer = Transformable.IdentitySimpleTransformer
  ): Picture =
    require(numberOfReplicas >= 0, s"Number of replicas cannot be negative (was $numberOfReplicas)")

    @tailrec
    def replicate(
        replicasLeft: Int,
        previousTransformedPicture: Picture,
        resultPicture: Picture
    ): Picture =

      if replicasLeft == 0 then return resultPicture

      val transformed = transformer(previousTransformedPicture)

      replicate(
        replicasLeft - 1,
        transformed,
        resultPicture.addTo(side, transformed, padding, alignment)
      )
    end replicate

    replicate(
      numberOfReplicas,
      previousTransformedPicture = this.asPicture,
      resultPicture = this.asPicture
    )
  end replicateTo

  // Moving
  // -------------------------------------------------------------------------------------------- \\

  /** Moves the transformable object to a specified position, with the reference point determined by
    * `positionType` (center or upper left corner).
    *
    * @param x
    *   The x-coordinate of the new position.
    * @param y
    *   The y-coordinate of the new position.
    * @param positionType
    *   The reference point for the new position: center or upper left corner.
    * @return
    *   A new instance of `TransformableType`, positioned as specified.
    */
  def moveTo(
      x: Double,
      y: Double,
      positionType: PositionType
  ): TransformableType =
    positionType match
      case PositionType.Center =>
        moveCenterTo(x, y)

      case PositionType.UpperLeftCorner =>
        moveUpperLeftCornerTo(x, y)

  /** Moves the center of the transformable object to the specified coordinates.
    *
    * @param x
    *   The x-coordinate of the new center position.
    * @param y
    *   The y-coordinate of the new center position.
    * @return
    *   A new instance of `TransformableType` with the center positioned as specified.
    */
  def moveCenterTo(x: Double, y: Double): TransformableType =
    moveBy(x - boundary.center.x, y - boundary.center.y)

  /** Moves the center of the transformable object to the specified position.
    *
    * @param position
    *   The new center position.
    * @return
    *   A new instance of `TransformableType` with the center positioned as specified.
    */
  def moveCenterTo(position: Pos): TransformableType =
    moveCenterTo(position.x, position.y)

  /** Moves the upper left corner of the transformable object to the specified coordinates.
    *
    * @param x
    *   The x-coordinate of the new upper left corner.
    * @param y
    *   The y-coordinate of the new upper left corner.
    * @return
    *   A new instance of `TransformableType` with the upper left corner positioned as specified.
    */
  def moveUpperLeftCornerTo(x: Double, y: Double): TransformableType =
    moveBy(x - boundary.upperLeftCorner.x, y - boundary.upperLeftCorner.y)

  /** Moves the upper left corner of the transformable object to the specified position.
    *
    * @param position
    *   The new upper left corner position.
    * @return
    *   A new instance of `TransformableType` with the upper left corner positioned as specified.
    */
  def moveUpperLeftCornerTo(position: Pos): TransformableType =
    moveUpperLeftCornerTo(position.x, position.y)

  /** Moves the transformable object by the specified offsets from its current position.
    *
    * @param xOffset
    *   The horizontal offset by which to move the object.
    * @param yOffset
    *   The vertical offset by which to move the object.
    * @return
    *   A new instance of `TransformableType` offset from the original position as specified.
    */
  def moveBy(xOffset: Double, yOffset: Double): TransformableType =
    val newPos = position + (xOffset, yOffset)
    copy(newPosition = newPos)

  // Scale using both target width and target height
  // -------------------------------------------------------------------------------------------- \\

  /** Scales the transformable object to a specified width and height relative to its center.
    *
    * @param targetWidth
    *   The target width in pixels.
    * @param targetHeight
    *   The target height in pixels.
    * @return
    *   A new instance of the transformable object scaled to the specified dimensions.
    */
  def scaleTo(targetWidth: Double, targetHeight: Double): TransformableType =
    scaleTo(targetWidth, targetHeight, position)

  /** Scales this object to given width and height in relation to a given point.
    * @param targetWidth
    *   The target width in pixels.
    * @param targetHeight
    *   The target height in pixels.
    * @param relativityPoint
    *   The point relative to which the scaling is applied.
    * @return
    *   A new instance of the transformable object scaled to the specified dimensions.
    */
  def scaleTo(targetWidth: Double, targetHeight: Double, relativityPoint: Pos): TransformableType =
    val horizontalFactor = targetWidth / boundary.width.inPixels
    val verticalFactor   = targetHeight / boundary.height.inPixels

    scaleBy(
      horizontalFactor = horizontalFactor,
      verticalFactor = verticalFactor,
      relativityPoint = relativityPoint
    )

  //  // Scale using a given factor for both horizontal and vertical directions
  //  // -------------------------------------------------------------------------------------------- \\
  //

  /** Scales the transformable object uniformly in both horizontal and vertical directions relative
    * to its center.
    *
    * @param factor
    *   The scaling factor to apply uniformly to both dimensions.
    * @return
    *   A new instance of the transformable object scaled by the specified factor.
    */
  def scaleBy(factor: Double): TransformableType =
    scaleBy(factor, factor)

  /** Scales the transformable object uniformly in both horizontal and vertical directions relative
    * to a specified point.
    *
    * @param factor
    *   The scaling factor to apply uniformly to both dimensions.
    * @param relativityPoint
    *   The point relative to which the scaling is applied.
    * @return
    *   A new instance of the transformable object scaled by the specified factor around the given
    *   point.
    */
  def scaleBy(factor: Double, relativityPoint: Pos): TransformableType =
    scaleBy(factor, factor, relativityPoint)

  // Scale using both horizontal and vertical factors
  // -------------------------------------------------------------------------------------------- \\

  /** Scales this object by given horizontal and vertical factors in relation to its center.
    *
    * @param horizontalFactor
    *   The factor by which to scale the object horizontally.
    * @param verticalFactor
    *   The factor by which to scale the object vertically.
    * @return
    *   A new instance of the transformable object scaled by the specified factors.
    */
  def scaleBy(horizontalFactor: Double, verticalFactor: Double): TransformableType

  /** Scales this object by given horizontal and vertical factors in relation to a specified point.
    *
    * @param horizontalFactor
    *   The factor by which to scale the object horizontally.
    * @param verticalFactor
    *   The factor by which to scale the object vertically.
    * @param relativityPoint
    *   The point in relation to which the object is scaled.
    * @return
    *   A new instance of the transformable object scaled by the specified factors.
    */
  def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): TransformableType

  // Cropping
  // -------------------------------------------------------------------------------------------- \\

  /** Crops the transformable object to a specified boundary.
    *
    * @param boundary
    *   The boundary to crop the transformable object to.
    * @return
    *   A Bitmap representing the cropped area of the transformable object.
    */
  inline final def crop(boundary: Bounds): Bitmap =
    crop(boundary.upperLeftCorner, boundary.lowerRightCorner)

  /** Crops the transformable object using specified upper left and lower right corners.
    *
    * @param upperLeftCorner
    *   The upper left corner of the crop area.
    * @param lowerRightCorner
    *   The lower right corner of the crop area.
    * @return
    *   A Bitmap representing the cropped area of the transformable object.
    */
  inline final def crop(upperLeftCorner: Pos, lowerRightCorner: Pos): Bitmap =
    crop(upperLeftCorner.x, upperLeftCorner.y, lowerRightCorner.x, lowerRightCorner.y)

  /** Crops the transformable object using an upper left corner and specified dimensions.
    *
    * @param upperLeftCorner
    *   The upper left corner of the crop area.
    * @param width
    *   The width of the crop area in pixels.
    * @param height
    *   The height of the crop area in pixels.
    * @return
    *   A Bitmap representing the cropped area of the transformable object.
    */
  inline final def crop(
      upperLeftCorner: Pos,
      width: Double,
      height: Double
  ): Bitmap =
    crop(
      upperLeftCorner.x,
      upperLeftCorner.y,
      upperLeftCorner.x + width,
      upperLeftCorner.y + height
    )

  /** Crops this object to a specified area defined by upper left and lower right coordinates.
    *
    * @param upperLeftX
    *   The x-coordinate of the upper left corner of the crop area.
    * @param upperLeftY
    *   The y-coordinate of the upper left corner of the crop area.
    * @param lowerRightX
    *   The x-coordinate of the lower right corner of the crop area.
    * @param lowerRightY
    *   The y-coordinate of the lower right corner of the crop area.
    * @return
    *   A bitmap representing the cropped area of the object.
    */
  def crop(
      upperLeftX: Double,
      upperLeftY: Double,
      lowerRightX: Double,
      lowerRightY: Double
  ): Bitmap =
    toBitmap.crop(upperLeftX, upperLeftY, lowerRightX, lowerRightY)

  // Rotating
  // -------------------------------------------------------------------------------------------- \\

  /** Rotates this object around a specified point by a given angle.
    *
    * @param angle
    *   The angle in degrees to rotate the object.
    * @param centerOfRotation
    *   The point around which the object is rotated.
    * @return
    *   A new instance of the transformable object rotated by the specified angle.
    */
  def rotateBy(angle: Double, centerOfRotation: Pos): TransformableType

  /** Rotates this object around a given point by 90 degrees clockwise.
    */
  def rotateBy90DegreesCW(centerOfRotation: Pos): TransformableType =
    rotateBy(90.0, centerOfRotation)

  /** Rotates this object around a given point by 90 degrees counterclockwise.
    */
  def rotateBy90DegreesCCW(centerOfRotation: Pos): TransformableType =
    rotateBy(-90.0, centerOfRotation)

  /** Rotates the transformable object around its center by a specified angle in degrees.
    *
    * @param angle
    *   The angle in degrees to rotate the object.
    * @return
    *   A new instance of the transformable object rotated by the specified angle.
    */
  def rotateByAroundOrigin(angle: Double): TransformableType

end Transformable
