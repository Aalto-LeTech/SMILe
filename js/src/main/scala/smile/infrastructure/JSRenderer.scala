package smile.infrastructure

import org.scalajs.dom
import smile.RenderSettings
import smile.pictures.Element

/** Renders elements into bitmaps and SVGs in the browser. Scala.js only. It is a stable entry point
  * of `smile.infrastructure`, as [[smile.infrastructure.Renderer]] is.
  *
  * Both go through the shared draw plan. Bitmaps are drawn on a canvas by
  * `smile.infrastructure.CanvasPlanRenderer` and SVGs are built by
  * `smile.infrastructure.SvgPlanRenderer`, so that they agree with each other and with the JVM.
  */
object JSRenderer extends Renderer:

  /** Draws `picture` onto `target` with `scaleFactor` canvas pixels per logical pixel. It draws
    * what drawing the bitmap [[smile.infrastructure.Renderer.createBitmapFrom]] gives for the same
    * arguments at the context's translation would draw, and makes no bitmap. On a context whose
    * transform only translates, it draws as `target.drawImage(bitmap.toCanvas(settings), 0, 0)`
    * does.
    *
    * The drawing is on the pixels of the target's canvas. The upper left corner of the bitmap lands
    * where the translation of the context's transform is, rounded to the nearest whole pixel, with
    * a half going to the even one. That corner is the corner of the picture's bounds, covered by
    * whole canvas pixels, which is seldom the picture's origin. A circle is centered on its origin,
    * so its bounds start above and to the left of it. The rest of the transform is not used, so a
    * context whose transform scales by the display's `devicePixelRatio` takes that ratio as
    * `scaleFactor`. Only the context's clip is drawn into, and its state is left as it was. The
    * empty picture draws nothing.
    *
    * To place the picture's origin on the translation instead, translate the context further by the
    * upper left corner of the picture's [[smile.pictures.Element.bounds]], in canvas pixels, or in
    * logical pixels on a context whose transform already scales by `scaleFactor`. The origin then
    * lands exactly there when that corner is on whole canvas pixels. Otherwise it can land one
    * pixel off, because the bitmap's corner is the bounds' corner rounded down to a whole pixel,
    * while the translation is rounded to the nearest one.
    *
    * @param scaleFactor
    *   Canvas pixels per logical pixel. Must be positive and finite, and 1 when the settings ask
    *   for `ResamplingMode.Stepwise`.
    * @param settings
    *   The settings the picture is drawn with. See [[smile.RenderSettings]].
    * @param timeMillis
    *   The instant the picture is drawn at, as for
    *   [[smile.infrastructure.Renderer.createBitmapFrom]].
    */
  def renderInto(
      target: dom.CanvasRenderingContext2D,
      picture: Element,
      scaleFactor: Double = 1.0,
      settings: RenderSettings = RenderSettings.Default,
      timeMillis: Long = System.currentTimeMillis()
  ): Unit =
    Renderer.requireScaleFactor(scaleFactor, settings)
    CanvasPlanRenderer.renderInto(target, picture, scaleFactor, settings, timeMillis)

  /** An SVG of `picture`, whose `viewBox` is the whole logical pixels the picture's bounds cover,
    * as its bitmap is, and which stays sharp at any size it is shown at. The empty picture is one
    * transparent pixel.
    *
    * @param settings
    *   The settings to render with. An SVG consults how the bitmaps in the picture are to be
    *   resampled wherever it is shown at another size than theirs.
    * @param density
    *   How many pixels per logical pixel the rasters the SVG embeds have, such as a bitmap of a
    *   picture. 2 keeps them sharp on a display with two device pixels per logical pixel. Vectors
    *   are sharp at any density.
    * @param timeMillis
    *   The instant the picture is drawn at, as for
    *   [[smile.infrastructure.Renderer.createBitmapFrom]]. Each animation in it shows its frame at
    *   that instant.
    * @throws IllegalArgumentException
    *   if the density is not positive and finite
    */
  def createSvgFrom(
      picture: Element,
      settings: RenderSettings = RenderSettings.Default,
      density: Double = 1.0,
      timeMillis: Long = System.currentTimeMillis()
  ): dom.Element =
    SvgPlanRenderer.render(picture, settings, density, timeMillis)

  /** The markup of the SVG that [[smile.infrastructure.JSRenderer.createSvgFrom]] makes. */
  def svgString(
      picture: Element,
      settings: RenderSettings = RenderSettings.Default,
      density: Double = 1.0,
      timeMillis: Long = System.currentTimeMillis()
  ): String =
    createSvgFrom(picture, settings, density, timeMillis).outerHTML
end JSRenderer
