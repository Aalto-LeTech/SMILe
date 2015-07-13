package aalto.smcl.images

import java.awt.{ Graphics2D => JGraphics2D }
import aalto.smcl.images.operations.BitmapOperation

/**
 * Ensures that the Java's `Graphics2D` API is available for use.
 *
 * @author Aleksi Lukkarinen
 */
private[images] trait OperableBitmap {

  /**
   *  Java's `Graphics2D` interface to enable graphic processing.
   */
  private[images] def graphics2D: JGraphics2D

  /**
   * Applies a bitmap operation to a bitmap.
   */
  def applyOperation(operation: BitmapOperation): Unit

}
