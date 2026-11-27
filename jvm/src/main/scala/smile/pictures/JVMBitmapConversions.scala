package smile.pictures

import smile.RenderSettings

import java.awt.image.BufferedImage
import javax.swing.ImageIcon

/** JVM-only conversions of a [[smile.pictures.Bitmap]] to the AWT and Swing image types, for
  * handing SMILe's output to Swing GUIs and other Java image APIs.
  *
  * They are extension methods because `Bitmap` is shared with Scala.js, which has no AWT.
  * `import smile.pictures.*` brings them into scope.
  */
extension (bitmap: Bitmap)
  /** This bitmap's pixels as a new `TYPE_INT_ARGB` `BufferedImage` of the bitmap's size. JVM only.
    *
    * The image is a copy. Drawing into it leaves the bitmap unchanged, as an immutable `Bitmap`
    * requires. It copies the packed ARGB values, because drawing the bitmap would drop the color of
    * fully transparent pixels. A `BufferedImage` is also a `java.awt.Image`.
    *
    * @param settings
    *   The settings the bitmap's pixels are computed with. See [[smile.pictures.Bitmap.pixels]].
    */
  def toAWTImage(settings: RenderSettings = RenderSettings.Default): BufferedImage =
    bitmap.nativeBuffer(settings).copiedImage

  /** This bitmap as a Swing `ImageIcon` the size of the bitmap, painting a copy of its pixels. JVM
    * only. See [[smile.pictures.toAWTImage]].
    */
  def toSwingIcon(settings: RenderSettings = RenderSettings.Default): ImageIcon =
    new ImageIcon(toAWTImage(settings))
