/* .            .           .                   .                 +             .          +      */
/*         +-----------+  +---+    +  +---+  +-----------+  +---+    Media Programming in Scala   */
/*   *     |           |  |    \     /    |  |           | +|   |            Since 2015           */
/*         |   +-------+  |     \   /     |  |   +-------+  |   |   .                        .    */
/*         |   |          |      \ /      |  |   |          |   |         Aalto University        */
/*       . |   +-------+  |   .   V   .   |  |   |   .      |   |      .   Espoo, Finland       . */
/*  +      |           |  |   |\     /|   |  |   |          |   |                  .    +         */
/*         +------+    |  |   | \   / |   |  |   |          |   |    +        *                   */
/*    *           |    |  |   |  \ /  |   |  |   |      *   |   |                     .      +    */
/*      -- +------+    |  |   |   V  *|   |  |   +-------+  |   +-------+ --    .                 */
/*    ---  |           |  |   | .     |   |  |           |  |           |  ---      +      *      */
/*  ------ +-----------+  +---+       +---+  +-----------+  +-----------+ ------               .  */
/*                                                                                     .          */
/*     T H E   S C A L A   M E D I A   C O M P U T A T I O N   L I B R A R Y      .         +     */
/*                                                                                    *           */

package smcl.pictures


import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

import smcl.infrastructure.Identity
import smcl.modeling.d2._
import smcl.settings._
import smcl.viewers.{display => displayInViewer}




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
trait PictureElement
    extends HasPos
        with HasBounds
        with HasDims
        with TypeQueryable
        with Movable[PictureElement]
        with Rotatable[PictureElement]
        with Scalable[PictureElement]
        with Cropable[Bitmap] {

  /** */
  type SimpleTransformer = PictureElement => PictureElement

  /** */
  val IdentitySimpleTransformer: SimpleTransformer = (i: PictureElement) => i


  /**
   * Position of this [[PictureElement]].
   *
   * @return
   */
  @inline
  def position: Pos = boundary.center

  /**
   *
   *
   * @return
   */
  def points: Seq[Pos] = Seq()

  /**
   *
   *
   * @return
   */
  def identity: Identity

  /**
   * Tells if this [[PictureElement]] can be rendered on a bitmap.
   *
   * @return
   */
  def isRenderable: Boolean

  /**
   *
   *
   * @return
   */
  @inline
  def toBitmap: Bitmap = Bitmap(this)

  /**
   *
   *
   * @return
   */
  @inline
  def toPicture: Picture = Picture(this)

  /**
   *
   */
  @inline
  def display(): PictureElement = {
    displayInViewer(toBitmap)

    this
  }

  /**
   *
   *
   * @param upperLeftCornerX
   * @param upperLeftCornerY
   * @param lowerRightCornerX
   * @param lowerRightCornerY
   *
   * @return
   */
  @inline
  override
  def crop(
      upperLeftCornerX: Double,
      upperLeftCornerY: Double,
      lowerRightCornerX: Double,
      lowerRightCornerY: Double): Bitmap = {

    toBitmap.crop(
      upperLeftCornerX,
      upperLeftCornerY,
      lowerRightCornerX,
      lowerRightCornerY)
  }

  /**
   *
   *
   * @param content
   *
   * @return
   */
  @inline
  def addToBack(content: PictureElement): PictureElement = addToBack(Seq(content))

  /**
   *
   *
   * @param content
   *
   * @return
   */
  @inline
  def addToBack(content: Seq[PictureElement]): PictureElement =
    Picture(appendTo(content, Seq(this)))

  /**
   *
   *
   * @param contentToAppend
   * @param existingContent
   *
   * @return
   */
  @inline
  protected final
  def appendTo(
      contentToAppend: Seq[PictureElement],
      existingContent: Seq[PictureElement]): Seq[PictureElement] = {

    contentToAppend.foldLeft(existingContent){(allElements, currentElement) =>
      if (currentElement.isPicture)
        allElements ++ currentElement.toPicture.elements
      else
        allElements :+ currentElement
    }
  }

  /**
   *
   *
   * @param content
   *
   * @return
   */
  @inline
  def addToFront(content: PictureElement): PictureElement = content.addToBack(this)

  /**
   *
   *
   * @param content
   *
   * @return
   */
  @inline
  def addToFront(content: Seq[PictureElement]): PictureElement =
    Picture(prependTo(content, Seq(this)))

  /**
   *
   *
   * @param contentToPrepend
   * @param existingContent
   *
   * @return
   */
  @inline
  protected final
  def prependTo(
      contentToPrepend: Seq[PictureElement],
      existingContent: Seq[PictureElement]): Seq[PictureElement] = {

    contentToPrepend.foldRight(existingContent){(currentElement, allElements) =>
      if (currentElement.isPicture)
        currentElement.toPicture.elements ++ allElements
      else
        currentElement +: allElements
    }
  }

  /**
   *
   *
   * @param content
   *
   * @return
   */
  @inline
  def +: (content: PictureElement): PictureElement = addToFront(content)

  /**
   *
   *
   * @param content
   *
   * @return
   */
  @inline
  def :+ (content: PictureElement): PictureElement = addToBack(content)

  /**
   *
   *
   * @param content
   * @param paddingInPixels
   * @param alignment
   *
   * @return
   */
  @inline
  def addToTop(
      content: PictureElement,
      paddingInPixels: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment): PictureElement = {

    val newUpperLeftCornerX = boundary.horizontalPositionFor(alignment, content.boundary)

    val newUpperLeftCornerY =
      boundary.upperLeftMarker.yInPixels - paddingInPixels - content.boundary.height.inPixels

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY)
  }

  /**
   *
   *
   * @param content
   * @param paddingInPixels
   * @param alignment
   *
   * @return
   */
  @inline
  def addToRight(
      content: PictureElement,
      paddingInPixels: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment): PictureElement = {

    val newUpperLeftCornerX = boundary.lowerRightMarker.xInPixels + paddingInPixels
    val newUpperLeftCornerY = boundary.verticalPositionFor(alignment, content.boundary)

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY)
  }

  /**
   *
   *
   * @param content
   * @param paddingInPixels
   * @param alignment
   *
   * @return
   */
  @inline
  def addToBottom(
      content: PictureElement,
      paddingInPixels: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment): PictureElement = {

    val newUpperLeftCornerX = boundary.horizontalPositionFor(alignment, content.boundary)
    val newUpperLeftCornerY = boundary.lowerRightMarker.yInPixels + paddingInPixels

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY)
  }

  /**
   *
   *
   * @param content
   * @param paddingInPixels
   * @param alignment
   *
   * @return
   */
  @inline
  def addToLeft(
      content: PictureElement,
      paddingInPixels: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment): PictureElement = {

    val newUpperLeftCornerX =
      boundary.upperLeftMarker.xInPixels - paddingInPixels - content.boundary.width.inPixels

    val newUpperLeftCornerY = boundary.verticalPositionFor(alignment, content.boundary)

    addAt(content, newUpperLeftCornerX, newUpperLeftCornerY)
  }

  /**
   *
   *
   * @param content
   * @param positions
   *
   * @return
   */
  @inline
  def addCopiesAtPos(
      content: PictureElement,
      positions: Seq[Pos]): PictureElement = {

    val wholeContent = ListBuffer[PictureElement]()
    val mover =
      if (content.isPicture)
        wholeContent ++= content.moveTo(_: Pos).toPicture.elements
      else
        wholeContent += content.moveTo(_: Pos)

    positions.foreach(mover)
    addToFront(wholeContent)
  }

  /**
   *
   *
   * @param content
   * @param positions
   *
   * @return
   */
  @inline
  def addCopiesAt(
      content: PictureElement,
      positions: Seq[(Double, Double)]): PictureElement = {

    val wholeContent = ListBuffer[PictureElement]()
    val mover =
      if (content.isPicture) {
        wholeContent ++= content.moveTo(_: Double, _: Double).toPicture.elements
      }
      else {
        wholeContent += content.moveTo(_: Double, _: Double)
      }

    positions.foreach(coords => mover(coords._1, coords._2))
    addToFront(wholeContent)
  }

  /**
   *
   *
   * @param contentsAndPositions
   *
   * @return
   */
  @inline
  def addAtPos(contentsAndPositions: Seq[(PictureElement, Pos)]): PictureElement = {
    val movedContent = contentsAndPositions.map{params =>
      params._1.moveTo(params._2.xInPixels, params._2.yInPixels)
    }

    addToFront(movedContent)
  }

  /**
   *
   *
   * @param contentAndPosition
   *
   * @return
   */
  @inline
  def addAtPos(contentAndPosition: (PictureElement, Pos)): PictureElement =
    addAtPos(
      contentAndPosition._1,
      contentAndPosition._2)

  /**
   *
   *
   * @param content
   * @param position
   *
   * @return
   */
  @inline
  def addAtPos(
      content: PictureElement,
      position: Pos): PictureElement = {

    addAt(
      content,
      position.xInPixels,
      position.yInPixels)
  }

  /**
   *
   *
   * @param contentsAndCoordinatesInPixels
   *
   * @return
   */
  @inline
  def addAt(contentsAndCoordinatesInPixels: Seq[(PictureElement, Double, Double)]): PictureElement = {
    val movedContent = contentsAndCoordinatesInPixels.map{params =>
      params._1.moveTo(params._2, params._3)
    }

    addToFront(movedContent)
  }

  /**
   *
   *
   * @param contentAndCoordinatesInPixels
   *
   * @return
   */
  @inline
  def addAt(contentAndCoordinatesInPixels: (PictureElement, Double, Double)): PictureElement =
    addAt(
      contentAndCoordinatesInPixels._1,
      contentAndCoordinatesInPixels._2,
      contentAndCoordinatesInPixels._3)

  /**
   *
   *
   * @param content
   * @param xCoordinate
   * @param yCoordinate
   *
   * @return
   */
  @inline
  def addAt(
      content: PictureElement,
      xCoordinate: Double,
      yCoordinate: Double): PictureElement = {

    addToFront(content.moveTo(xCoordinate, yCoordinate))
  }

  /**
   *
   *
   * @param numberOfReplicas
   * @param paddingInPixels
   * @param alignment
   * @param transformer
   *
   * @return
   */
  def replicateHorizontally(
      numberOfReplicas: Int,
      paddingInPixels: Double = DefaultPaddingInPixels,
      alignment: VerticalAlignment = DefaultVerticalAlignment,
      transformer: SimpleTransformer = IdentitySimpleTransformer): PictureElement = {

    if (numberOfReplicas < 0) {
      throw new IllegalArgumentException(
        s"Number of replicas cannot be negative (was $numberOfReplicas)")
    }

    @tailrec
    def replicate(
        replicasLeft: Int,
        previousTransformedPicture: PictureElement,
        resultPicture: PictureElement): PictureElement = {

      if (replicasLeft == 0)
        return resultPicture

      val transformed = transformer(previousTransformedPicture)

      replicate(
        replicasLeft - 1,
        transformed,
        resultPicture.addToRight(transformed, paddingInPixels, alignment))
    }

    replicate(numberOfReplicas, this, this)
  }

  /**
   *
   *
   * @param numberOfReplicas
   * @param paddingInPixels
   * @param alignment
   * @param transformer
   *
   * @return
   */
  def replicateVertically(
      numberOfReplicas: Int,
      paddingInPixels: Double = DefaultPaddingInPixels,
      alignment: HorizontalAlignment = DefaultHorizontalAlignment,
      transformer: SimpleTransformer = IdentitySimpleTransformer): PictureElement = {

    if (numberOfReplicas < 0) {
      throw new IllegalArgumentException(
        s"Number of replicas cannot be negative (was $numberOfReplicas)")
    }

    @tailrec
    def replicate(
        replicasLeft: Int,
        previousTransformedPicture: PictureElement,
        resultPicture: PictureElement): PictureElement = {

      if (replicasLeft == 0)
        return resultPicture

      val transformed = transformer(previousTransformedPicture)

      replicate(
        replicasLeft - 1,
        transformed,
        resultPicture.addToBottom(transformed, paddingInPixels, alignment))
    }

    replicate(numberOfReplicas, this, this)
  }

}
