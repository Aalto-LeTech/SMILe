package smile.pictures

import smile.infrastructure.ResourceFactory
import smile.modeling.{Bounds, Pos}

object AnimatedPicture:
  /** Creates an `AnimatedPicture` from a GIF file located at the specified path.
    *
    * @param sourceResourcePath
    *   The path to the source GIF file.
    * @return
    *   An `AnimatedPicture` instance created from the GIF file.
    */
  def fromGifPath(sourceResourcePath: String): AnimatedPicture =
    ResourceFactory().animatedPictureFromPath(sourceResourcePath)

/** Represents an animated picture, consisting of a sequence of frames (PictureElements) and their
  * display durations.
  *
  * @param frames
  *   A sequence of `PictureElement` instances representing the frames of the animation.
  * @param frameDelays
  *   A sequence of integers representing the delay in milliseconds before showing the next frame.
  * @param currentFrameIndex
  *   The index of the currently displayed frame.
  * @param lastFrameTime
  *   The system time in milliseconds when the last frame was set. Defaults to the current system
  *   time.
  */
class AnimatedPicture(
    val frames: Seq[DrawableElement],
    val frameDelays: Seq[Int],
    val currentFrameIndex: Int = 0,
    val lastFrameTime: Long = System.currentTimeMillis()
) extends PictureElement:

  /** Attempts to advance to the next frame based on the elapsed time since the last frame update.
    *
    * @return
    *   A new `AnimatedPicture` instance, possibly with an updated frame index, if the delay for the
    *   current frame has passed.
    */
  def tryAdvanceFrame: AnimatedPicture =
    val nextIndex = (currentFrameIndex + 1) % frames.size
    val now       = System.currentTimeMillis()
    if now - lastFrameTime >= frameDelays(currentFrameIndex) then
      internalCopy(newIndex = nextIndex, newFrameTime = now)
    else this

  override lazy val boundary: Bounds = frames(currentFrameIndex).boundary

  override def copy(newPosition: Pos): AnimatedPicture =
    new AnimatedPicture(frames.map(_.copy(newPosition)), frameDelays, currentFrameIndex)

  private def internalCopy(
      newFrames: Seq[DrawableElement] = frames,
      newIndex: Int = currentFrameIndex,
      newFrameTime: Long = lastFrameTime
  ): AnimatedPicture =
    new AnimatedPicture(newFrames, frameDelays, newIndex, newFrameTime)

  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double
  ): AnimatedPicture =
    internalCopy(frames.map(_.scaleBy(horizontalFactor, verticalFactor)))

  override def scaleBy(
      horizontalFactor: Double,
      verticalFactor: Double,
      relativityPoint: Pos
  ): AnimatedPicture =
    internalCopy(frames.map(_.scaleBy(horizontalFactor, verticalFactor, relativityPoint)))

  override def rotateBy(
      angle: Double,
      centerOfRotation: Pos
  ): AnimatedPicture =
    internalCopy(frames.map(_.rotateBy(angle, centerOfRotation)))

  override def rotateByAroundOrigin(angle: Double): AnimatedPicture =
    internalCopy(frames.map(_.rotateByAroundOrigin(angle)))
