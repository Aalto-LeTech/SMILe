package smile

/** How a bitmap is resampled when a picture is scaled or rotated for an output.
  *
  * A bitmap records the operations done to it (see [[smile.pictures.Bitmap]]). The mode says what
  * an output does with them.
  */
enum ResamplingMode derives CanEqual:

  /** Resample once, from the source straight to the target. The source's pixels are drawn with
    * every scaling and turning composed into one map, at the output's own scale. A bitmap of a
    * picture has the picture rendered at the density the output needs. A bitmap that is only moved,
    * flipped, cropped, shifted or turned by right angles is drawn on whole pixels, as in
    * `Stepwise`.
    */
  case Direct

  /** Resample after each transformation in turn, at one buffer pixel per logical pixel, with the
    * buffer algorithms that once applied each operation at once. An output in this mode renders at
    * a scale factor of 1.
    */
  case Stepwise
end ResamplingMode

/** The settings an output consults when it renders a picture. They belong to the output.
  * Construction defaults, such as where a shape is centered, are plain default arguments of the
  * members that build pictures. There is no global configuration and no implicit instance. Every
  * member that renders takes the settings as an ordinary parameter, defaulting to
  * [[smile.RenderSettings.Default]], as in this example.
  *
  * ```
  * val bitmap = picture.toBitmap(RenderSettings(drawingIsAntiAliased = false))
  * ```
  *
  * @param drawingIsAntiAliased
  *   Whether shapes and text are drawn with antialiasing.
  * @param normalizesStrokes
  *   Whether Java2D may normalize outlines with `RenderingHints.VALUE_STROKE_NORMALIZE`, its own
  *   default. When false, it draws them where their geometry says, with `VALUE_STROKE_PURE`. Only
  *   the JVM renderer reads this setting. Without antialiasing, normalization nudges the edges of
  *   fills and strokes up to half a pixel down and to the right, so ink can leave the boundary at
  *   the bottom and right, where a bitmap sized from the boundary cuts it off, and the JVM no
  *   longer draws edges where the canvas does. A shape turned by other than a right angle is drawn
  *   about half a pixel down and right of the point it turns around (SMILe issue #21). It is here
  *   for programs that must keep the pixels of renderers that normalized, as earlier versions of
  *   SMILe did.
  * @param bufferScalingMethod
  *   The interpolation used when a buffer is scaled, and when a renderer draws a bitmap at another
  *   size than its buffer's.
  * @param bufferTransformMethod
  *   The interpolation used when a buffer is affinely transformed.
  * @param resamplingMode
  *   How a bitmap is resampled when it is scaled or rotated more than once.
  */
final case class RenderSettings(
    drawingIsAntiAliased: Boolean = true,
    normalizesStrokes: Boolean = false,
    bufferScalingMethod: ScalingMethod = ScalingMethod.Smooth,
    bufferTransformMethod: TransformMethod = TransformMethod.Bilinear,
    resamplingMode: ResamplingMode = ResamplingMode.Direct
) derives CanEqual

object RenderSettings:

  /** The settings that apply when the caller passes none, with each field at its default. */
  val Default: RenderSettings = RenderSettings()
end RenderSettings
