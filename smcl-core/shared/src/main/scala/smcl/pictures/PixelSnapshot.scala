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


import smcl.infrastructure.BitmapBufferAdapter




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object PixelSnapshot {

  /**
   *
   *
   * @param source
   *
   * @return
   */
  def apply(source: PictureElement): AbstractPixelSnapshot = {
    val bitmapCopy = source.toBitmapCopy

    if (bitmapCopy.buffer.isEmpty
        || bitmapCopy.buffer.get.widthInPixels == 0
        || bitmapCopy.buffer.get.heightInPixels == 0) {

      return new NullPixelSnapshot(bitmapCopy)
    }

    new PixelSnapshot(bitmapCopy)
  }

}




/**
 *
 *
 * @param bitmap
 *
 * @author Aleksi Lukkarinen
 */
class PixelSnapshot protected[pictures](bitmap: Bitmap)
    extends AbstractPixelSnapshot {

  /** */
  private[this]
  val buffer: BitmapBufferAdapter = bitmap.buffer.get

  /** */
  val widthInPixels: Int = buffer.widthInPixels

  /** */
  val heightInPixels: Int = buffer.heightInPixels

  /** */
  private[this]
  var (_reds, _greens, _blues, _opacities) =
    buffer.colorComponentArrays

  /** */
  private[pictures]
  def reds: Array[Int] = _reds

  /** */
  private[pictures]
  def greens: Array[Int] = _greens

  /** */
  private[pictures]
  def blues: Array[Int] = _blues

  /** */
  private[pictures]
  def opacities: Array[Int] = _opacities

  /**
   *
   *
   * @return
   */
  def redComponentArray: Array[Int] = {
    checkForInvalidation()
    _reds.clone
  }

  /**
   *
   *
   * @return
   */
  def greenComponentArray: Array[Int] = {
    checkForInvalidation()
    _greens.clone
  }

  /**
   *
   *
   * @return
   */
  def blueComponentArray: Array[Int] = {
    checkForInvalidation()
    _blues.clone
  }

  /**
   *
   *
   * @return
   */
  def opacityComponentArray: Array[Int] = {
    checkForInvalidation()
    _opacities.clone
  }

  /**
   *
   *
   * @return
   */
  def componentArrays: (Array[Int], Array[Int], Array[Int], Array[Int]) = {
    checkForInvalidation()
    (redComponentArray, greenComponentArray, blueComponentArray, opacityComponentArray)
  }

  /**
   *
   *
   * @return
   */
  def setRedComponentArray(array: Array[Int]): Unit = {
    checkForInvalidation()
    checkComponentArrayLength(array, "red")

    _reds = array.clone
  }

  /**
   *
   *
   * @return
   */
  def setGreenComponentArray(array: Array[Int]): Unit = {
    checkForInvalidation()
    checkComponentArrayLength(array, "green")

    _greens = array.clone
  }

  /**
   *
   *
   * @return
   */
  def setBlueComponentArray(array: Array[Int]): Unit = {
    checkForInvalidation()
    checkComponentArrayLength(array, "blue")

    _blues = array.clone
  }

  /**
   *
   *
   * @return
   */
  def setOpacityComponentArray(array: Array[Int]): Unit = {
    checkForInvalidation()
    checkComponentArrayLength(array, "opacity")

    _opacities = array.clone
  }

  /**
   *
   *
   * @return
   */
  override
  def setComponentArrays(
      reds: Array[Int],
      greens: Array[Int],
      blues: Array[Int],
      opacities: Array[Int]): Unit = {

    checkForInvalidation()

    checkComponentArrayLength(reds, "red")
    checkComponentArrayLength(greens, "green")
    checkComponentArrayLength(blues, "blue")
    checkComponentArrayLength(opacities, "opacity")

    _reds = reds.clone
    _greens = greens.clone
    _blues = blues.clone
    _opacities = opacities.clone
  }

  /**
   *
   */
  def toBitmap: Bitmap = {
    checkForInvalidation()

    buffer.setColorComponentArrays(_reds, _greens, _blues, _opacities)
    val bitmap = Bitmap(buffer)

    invalidation.setDone()

    bitmap
  }

  /**
   *
   *
   * @param x
   * @param y
   *
   * @return
   */
  def pixel(
      x: Int,
      y: Int): Pixel = {

    checkForInvalidation()

    require(x >= 0 && x < widthInPixels,
      s"X coordinate is out or range ($minXInPixels..$maxXInPixels)")

    require(x >= 0 && x < widthInPixels,
      s"Y coordinate is out or range ($minYInPixels..$maxYInPixels)")

    Pixel(
      this,
      minXInPixels, maxXInPixels,
      minYInPixels, maxYInPixels,
      x, y)
  }

  /**
   *
   * @return
   */
  override
  def iterator: AbstractPixelSnapshotIterator = {
    checkForInvalidation()
    PixelSnapshotRightwardsDownwardsIterator(this)
  }

  /**
   *
   * @return
   */
  def downwardsLeftwardsIterator: AbstractPixelSnapshotIterator = {
    checkForInvalidation()
    PixelSnapshotDownwardsLeftwardsIterator(this)
  }

  /**
   *
   * @return
   */
  def leftwardsDownwardsIterator: AbstractPixelSnapshotIterator = {
    checkForInvalidation()
    PixelSnapshotLeftwardsDownwardsIterator(this)
  }

  /**
   *
   * @return
   */
  def leftwardsUpwardsIterator: AbstractPixelSnapshotIterator = {
    checkForInvalidation()
    PixelSnapshotLeftwardsUpwardsIterator(this)
  }

  /**
   *
   * @return
   */
  def downwardsRightwardsIterator: AbstractPixelSnapshotIterator = {
    checkForInvalidation()
    PixelSnapshotDownwardsRightwardsIterator(this)
  }

  /**
   *
   * @return
   */
  def rightwardsUpwardsIterator: AbstractPixelSnapshotIterator = {
    checkForInvalidation()
    PixelSnapshotRightwardsUpwardsIterator(this)
  }

  /**
   *
   * @return
   */
  def upwardsLeftwardsIterator: AbstractPixelSnapshotIterator = {
    checkForInvalidation()
    PixelSnapshotUpwardsLeftwardsIterator(this)
  }

  /**
   *
   * @return
   */
  def upwardsRightwardsIterator: AbstractPixelSnapshotIterator = {
    checkForInvalidation()
    PixelSnapshotUpwardsRightwardsIterator(this)
  }

}
