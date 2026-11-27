package smile.infrastructure

import smile.RenderSettings
import smile.pictures.Element

import java.awt.Graphics2D

/** JVM-only drawing of a picture onto a caller's Java2D graphics, such as those a Swing component
  * paints with, without a bitmap in between.
  *
  * It is an extension method because [[smile.infrastructure.Renderer]] is shared with Scala.js,
  * which has no AWT. `import smile.infrastructure.renderInto` brings it into scope.
  */
extension (renderer: Renderer)
  /** Draws `picture` onto `target` with `scaleFactor` device pixels per logical pixel. It draws
    * what drawing the bitmap [[smile.infrastructure.Renderer.createBitmapFrom]] gives for the same
    * arguments at the target's translation would draw, and makes no bitmap. On graphics whose
    * transform only translates, it draws as
    * `target.drawImage(bitmap.toAWTImage(settings), 0, 0, null)` does. JVM only.
    *
    * The drawing is on the target's device pixels. The upper left corner of the bitmap lands where
    * the translation of the target's transform is, rounded to the nearest whole device pixel, with
    * a half going to the even one. That corner is the corner of the picture's bounds, covered by
    * whole device pixels, which is seldom the picture's origin. A circle is centered on its origin,
    * so its bounds start above and to the left of it. The rest of the transform is not used, so a
    * target whose transform scales, as a Swing component's does on a high-DPI screen, takes that
    * scale as `scaleFactor`. Only the target's clip is drawn into. Its transform, clip, composite,
    * paint and hints are left as they were. The empty picture draws nothing.
    *
    * To place the picture's origin on the translation instead, translate the target further by the
    * upper left corner of the picture's [[smile.pictures.Element.bounds]], in device pixels, or in
    * logical pixels on a target whose transform already scales by `scaleFactor`. The origin then
    * lands exactly there when that corner is on whole device pixels. Otherwise it can land one
    * device pixel off, because the bitmap's corner is the bounds' corner rounded down to a whole
    * device pixel, while the translation is rounded to the nearest one.
    *
    * In a Swing component's `paintComponent`:
    *
    * ```
    * val g2d   = graphics.asInstanceOf[java.awt.Graphics2D]
    * val scale = g2d.getDeviceConfiguration.getDefaultTransform.getScaleX
    * Renderer().renderInto(g2d, picture, scale)
    * ```
    *
    * @param scaleFactor
    *   Device pixels per logical pixel. Must be positive and finite, and 1 when the settings ask
    *   for `ResamplingMode.Stepwise`.
    * @param settings
    *   The settings the picture is drawn with. See [[smile.RenderSettings]].
    * @param timeMillis
    *   The instant the picture is drawn at, as for
    *   [[smile.infrastructure.Renderer.createBitmapFrom]].
    */
  def renderInto(
      target: Graphics2D,
      picture: Element,
      scaleFactor: Double = 1.0,
      settings: RenderSettings = RenderSettings.Default,
      timeMillis: Long = System.currentTimeMillis()
  ): Unit =
    Renderer.requireScaleFactor(scaleFactor, settings)
    JVMPlanRenderer.renderInto(target, picture, scaleFactor, settings, timeMillis)
