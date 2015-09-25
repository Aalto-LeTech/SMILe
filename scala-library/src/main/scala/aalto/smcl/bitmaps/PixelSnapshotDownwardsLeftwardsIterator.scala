package aalto.smcl.bitmaps


import scala.collection.AbstractIterator




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
class PixelSnapshotDownwardsLeftwardsIterator(
  val relatedPixelSnapshot: PixelSnapshot) extends AbstractIterator[Pixel] {

  /** */
  lazy val widthInPixels: Int = relatedPixelSnapshot.widthInPixels

  /** */
  lazy val heightInPixels: Int = relatedPixelSnapshot.heightInPixels

  /** */
  val MinXInPixels: Int = 0

  /** */
  val MinYInPixels: Int = 0

  /** */
  lazy val MaxXInPixels: Int = widthInPixels - 1

  /** */
  lazy val MaxYInPixels: Int = heightInPixels - 1

  /** */
  private var _currentXInPixels: Int = MaxXInPixels

  /** */
  private var _currentYInPixels: Int = MinYInPixels

  /** */
  private def currentXInPixels: Int = _currentXInPixels

  /** */
  private def currentYInPixels: Int = _currentYInPixels


  /**
   *
   */
  private def advance(): Unit = {
    if (_currentXInPixels > MinXInPixels)
      _currentXInPixels -= 1
    else {
      _currentXInPixels = MaxXInPixels
      _currentYInPixels += 1
    }
  }

  /**
   *
   *
   * @return
   */
  def hasNext: Boolean =
    _currentYInPixels <= MaxYInPixels &&
      _currentXInPixels >= MinXInPixels

  /**
   *
   *
   * @return
   */
  def next(): Pixel = {
    if (!hasNext)
      return Iterator.empty.next()

    val nextResult: Pixel = new Pixel(
      relatedPixelSnapshot,
      MinXInPixels, MaxXInPixels,
      MinYInPixels, MaxYInPixels,
      _currentXInPixels, _currentYInPixels)

    advance()

    nextResult
  }

}
