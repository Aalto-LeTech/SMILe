package smile.parity

import smile.testing.*

import smile.{RenderSettings, ScalingMethod}
import smile.colors.Color
import smile.infrastructure.{DeviceGrid, DrawPlan}
import smile.modeling.Pos
import smile.pictures.Element

/** The three renderers a picture can come out of. */
enum Backend derives CanEqual:
  case Awt, Canvas, Svg

  def label: String = this match
    case Awt    => "awt"
    case Canvas => "canvas"
    case Svg    => "svg"

/** Something a case needs from the environment it is rendered in. A case whose needs are not met is
  * skipped with the need as the reason.
  */
enum Needs derives CanEqual:
  /** A layout engine for SVG text, which jsdom does not have. */
  case SvgTextLayout

  /** A font both platforms can find by one name. See [[smile.parity.ParityCases.FontCandidates]].
    */
  case CommonFont

  /** SVG pictures, whose documents a canvas has to decode before it can draw them. */
  case SvgImages

/** What a picture must show at one point, derived by hand from the documented behavior. Probes
  * catch a mistake every backend makes alike, which no comparison between backends can.
  */
enum Ink derives CanEqual:
  /** Nothing is painted here. */
  case Empty

  /** Exactly this color, fully covering the pixel. */
  case Solid(color: Color)

  /** A color within `tolerance` of this one on every channel, as at a point inside a gradient or a
    * translucent overlap.
    */
  case Near(color: Color, tolerance: Int)

  /** Fully opaque paint of any color. */
  case Opaque

/** One probe. The device pixel the world position `at` falls into must show `ink`, at each of
  * `scales`.
  *
  * A probe holds at every scale the case is rendered at unless it names fewer, as one has to where
  * the plan rounds to device pixels. A point is one device pixel at every scale, and a snapped
  * bitmap's corner moves to the nearest whole device pixel.
  */
final case class InkProbe(at: Pos, ink: Ink, scales: Seq[Double] = ParityCase.Scales)

/** How closely the backends `first` and `second` agree on a case at `scale`, where that differs
  * from what the case expects of them at its other scales. As a rule it is a
  * [[smile.testing.Expectation.KnownDiffers]] for a difference only that scale brings out.
  */
final case class AtScale(scale: Double, first: Backend, second: Backend, expectation: Expectation)

/** What crispness a case asks for at some of its scales. Every device pixel is either untouched or
  * exactly one of `palette`, so nothing was blended. A bitmap drawn as whole-pixel blocks meets it,
  * and one that was resampled smoothly or drawn half a pixel off does not.
  */
final case class Crisp(scales: Seq[Double], palette: Set[Color])

/** One picture of the catalog.
  *
  * @param id
  *   A short name, unique in the catalog, that names the tests and the report's directories.
  * @param catches
  *   The kind of drift between backends this picture is there to expose.
  * @param picture
  *   Builds the picture. A function, because a picture may hold a platform buffer.
  * @param probes
  *   Hand-derived expectations every backend's rendering has to meet.
  * @param canvasVsSvg
  *   How closely the two Scala.js backends agree, in a browser.
  * @param awtVsCanvas
  *   How closely the JVM's rendering agrees with the canvas one.
  * @param awtVsSvg
  *   How closely the JVM's rendering agrees with the SVG one.
  * @param translucent
  *   Whether the picture blends partly transparent paint, which each backend rounds its own way.
  * @param needs
  *   What the environment has to offer for the case to run.
  * @param scaling
  *   The bitmap scaling method the case is rendered under, where it is not the default. See
  *   [[smile.parity.ParityCase.settings]].
  * @param scales
  *   The output scales, device pixels per logical pixel, the case is rendered and compared at.
  *   Every backend renders at each of them, and the probes and expectations hold at each.
  * @param crisp
  *   Where the case's rendering must be made of whole-pixel blocks alone.
  * @param atScales
  *   What a pair of backends is expected to agree on at one scale, where it differs from the case's
  *   own expectation for the pair.
  *
  * The SVG backend plans its output at scale 1, since an SVG is resolution-independent and only the
  * rasters it embeds take a density. Shown at another scale, it is the plan for 1x drawn larger, so
  * where the plan rounds to device pixels, the SVG's edges are on whole logical pixels and the
  * bitmap backends' on whole device pixels, up to half a logical pixel apart, and a bitmap the plan
  * copies one to one at 1x is shown as blocks where the bitmap backends resample it. The harness
  * therefore compares SVG with the other two backends at 1x in every case, and at 2x and 1.5x only
  * where [[smile.parity.ParityCase.roundsToDevicePixels]] is false, which is where neither the plan
  * at that scale nor the one at 1x rounds to device pixels. There the SVG is rasterized at that
  * scale with its rasters embedded at that density, and is held to the same expectation as at 1x.
  */
final case class ParityCase(
    id: String,
    catches: String,
    picture: () => Element,
    probes: Seq[InkProbe],
    canvasVsSvg: Expectation,
    awtVsCanvas: Expectation,
    awtVsSvg: Expectation,
    translucent: Boolean = false,
    needs: Set[Needs] = Set.empty,
    scaling: Option[ScalingMethod] = None,
    scales: Seq[Double] = ParityCase.Scales,
    crisp: Option[Crisp] = None,
    atScales: Seq[AtScale] = Seq.empty
):
  /** The settings every renderer renders this case with, which are the defaults with the case's
    * scaling method where it has one.
    */
  def settings: RenderSettings =
    scaling.fold(RenderSettings.Default)(method => RenderSettings(bufferScalingMethod = method))

  /** How closely `first` and `second` agree on this case at 1x. */
  def expectationFor(first: Backend, second: Backend): Expectation =
    Set(first, second) match
      case pair if pair == Set(Backend.Canvas, Backend.Svg) => canvasVsSvg
      case pair if pair == Set(Backend.Awt, Backend.Canvas) => awtVsCanvas
      case pair if pair == Set(Backend.Awt, Backend.Svg)    => awtVsSvg
      case _                                                => Expectation.Exact

  /** How closely `first` and `second` agree on this case at `scale`, in device pixels. A
    * [[smile.testing.Expectation.Layout]]'s slack is in logical pixels, as far apart as two font
    * engines put the ink of a text whatever its size on the screen, and so it is that many device
    * pixels times `scale`.
    */
  def expectationFor(first: Backend, second: Backend, scale: Double): Expectation =
    def inDevicePixels(expectation: Expectation): Expectation = expectation match
      case Expectation.Layout(slack) => Expectation.Layout(slack * scale)
      case Expectation.KnownDiffers(reason, otherwise, bound, limits) =>
        Expectation.KnownDiffers(reason, inDevicePixels(otherwise), inDevicePixels(bound), limits)
      case other => other
    inDevicePixels(
      atScales
        .find(at => at.scale == scale && Set(at.first, at.second) == Set(first, second))
        .fold(expectationFor(first, second))(_.expectation)
    )

  /** Whether the case rounds to device pixels at `scale`. It does when the bitmap backends' plan of
    * the picture at that scale, or the SVG backend's, which is the plan at 1x, puts something on
    * whole device pixels. That is a point, an axis-aligned clip, a mask's or a turned clip's layer,
    * a wrap's tiles, a snapped bitmap, or a bitmap clipped to the device pixels its centers cover,
    * as `smile.infrastructure.DrawPlan.roundsToDevicePixels` decides. Such a case is compared with
    * SVG at 1x only, for the reason [[smile.parity.ParityCase]] gives.
    */
  def roundsToDevicePixels(scale: Double): Boolean =
    val picture = this.picture()
    Seq(scale, 1.0).distinct.exists: planned =>
      val (_, ops) = DrawPlan.of(picture, DeviceGrid(planned), settings, 0L)
      DrawPlan.roundsToDevicePixels(ops)

  /** The pairs of backends compared at `scale`. All three are compared at 1x, and at another scale
    * those with SVG only where the case does not round to device pixels there, for the reason
    * [[smile.parity.ParityCase]] gives.
    */
  def pairsAt(scale: Double): Seq[(Backend, Backend)] =
    lazy val rounds = roundsToDevicePixels(scale)
    ParityCase.Pairs.filter((first, second) =>
      scale == 1.0 || (first != Backend.Svg && second != Backend.Svg) || !rounds
    )

  /** The name of this case at `scale`, for tests and the report's directories. */
  def nameAt(scale: Double): String = s"$id-at-${ParityCase.label(scale)}"

object ParityCase:

  /** The scales every case is rendered at unless it says otherwise. At the fractional 1.5x neither
    * the device grid nor the logical one lies on the other.
    */
  val Scales: Seq[Double] = Seq(1.0, 2.0, 1.5)

  /** A scale as a short label such as `1x`, `2x` or `1.5x`. */
  def label(scale: Double): String =
    (if scale == math.rint(scale) then scale.toLong.toString else scale.toString) + "x"

  /** The three pairs of backends, in the order they are compared. */
  val Pairs: Seq[(Backend, Backend)] = Seq(
    Backend.Awt    -> Backend.Canvas,
    Backend.Awt    -> Backend.Svg,
    Backend.Canvas -> Backend.Svg
  )
