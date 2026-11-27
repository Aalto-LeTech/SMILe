package smile.pictures

import org.scalajs.dom.html

import smile.RenderSettings

/** Browser-only conversions of a [[smile.pictures.Bitmap]] to the DOM's image types, for handing
  * SMILe's output to other canvas code.
  *
  * They are extension methods because `Bitmap` is shared with the JVM, which has no DOM.
  * `import smile.pictures.*` brings them into scope.
  */
extension (bitmap: Bitmap)
  /** This bitmap's pixels on a new canvas of the bitmap's size. Scala.js only.
    *
    * Every call makes a new canvas with a copy of the pixels. Drawing on it leaves the bitmap
    * unchanged, as an immutable `Bitmap` requires. A canvas premultiplies its pixels by their
    * alpha, so the colors of translucent pixels read back from it may come back slightly changed.
    *
    * @param settings
    *   The settings the bitmap's pixels are computed with. See [[smile.pictures.Bitmap.pixels]].
    */
  def toCanvas(settings: RenderSettings = RenderSettings.Default): html.Canvas =
    bitmap.nativeBuffer(settings).newCanvas
