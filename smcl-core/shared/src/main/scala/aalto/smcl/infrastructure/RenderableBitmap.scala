package aalto.smcl.infrastructure




/**
 * Ensures that a bitmap can be rendered onto drawing surfaces.
 *
 * @author Aleksi Lukkarinen
 */
trait RenderableBitmap {

  /**
   * Renders this [[RenderableBitmap]] onto a drawing surface.
   */
  def renderOnto(platformDrawingSurface: DrawingSurfaceAdapter, x: Int, y: Int): Unit

  /**
   * Returns an instance of Java's [[BitmapBufferAdapter]]
   * representing this [[RenderableBitmap]].
   */
  def toRenderedRepresentation: BitmapBufferAdapter

}
