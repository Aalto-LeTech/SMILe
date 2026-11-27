package smile.infrastructure

import smile.colors.{Color, LinearGradient, Paint}
import smile.modeling.Pos
import smile.pictures.StrokeStyle

import java.awt

/** Conversions from SMILe's shared style and paint types to their AWT equivalents. Pure functions
  * with no drawing state, kept separate from the renderer that paints with them
  * (`smile.infrastructure.JVMPlanRenderer`), so "what to paint with" and "how to paint" stay
  * independent concerns.
  */
private[smile] object AwtConversions:

  def colorToAWT(color: Color): awt.Color =
    new awt.Color(color.red, color.green, color.blue, color.opacity)

  /** Converts a gradient into an AWT paint.
    *
    * AWT resolves a `LinearGradientPaint`'s coordinates in the user space that is in effect when
    * the shape is filled, so the renderer passes the paint origin of the operation it draws as
    * `elementOrigin` in that same space, which is already translated and rotated. That keeps
    * [[smile.colors.LinearGradient]]'s coordinates element-local on the JVM.
    */
  def linearGradientToAWT(gradient: LinearGradient, elementOrigin: Pos): awt.Paint =
    val (start, end) = gradient.anchoredAt(elementOrigin)

    // AWT takes the points as floats and refuses two equal ones.
    if LinearGradient.isTooShortToPaint(start, end) then colorToAWT(gradient.averageColor)
    else
      new awt.LinearGradientPaint(
        start.x.toFloat,
        start.y.toFloat,
        end.x.toFloat,
        end.y.toFloat,
        strictlyIncreasing(gradient.fractions),
        gradient.colors.map(colorToAWT).toArray,
        gradient.cycleMethod match
          case LinearGradient.CycleMethod.NoCycle => awt.MultipleGradientPaint.CycleMethod.NO_CYCLE
          case LinearGradient.CycleMethod.Reflect => awt.MultipleGradientPaint.CycleMethod.REFLECT
          case LinearGradient.CycleMethod.Repeat  => awt.MultipleGradientPaint.CycleMethod.REPEAT
      )

  /** A gradient's fractions as AWT wants them, as strictly increasing floats.
    * [[smile.colors.LinearGradient]] allows two stops at one fraction, a hard edge between two
    * colors, which the canvas and SVG take as it is. Two fractions closer than a float can tell
    * apart are one fraction here as well. The later of two equal fractions is moved up by the
    * smallest step a float has, or, where that would pass one, the earlier ones are moved down. The
    * difference is too small to see.
    */
  private def strictlyIncreasing(fractions: Seq[Double]): Array[Float] =
    val result = fractions.map(_.toFloat).toArray
    for index <- 1 until result.length do
      if result(index) <= result(index - 1) then result(index) = Math.nextUp(result(index - 1))
    if result.nonEmpty && result.last > 1.0f then
      result(result.length - 1) = 1.0f
      for index <- result.length - 2 to 0 by -1 do
        if result(index) >= result(index + 1) then result(index) = Math.nextDown(result(index + 1))
    result

  def paintToAWT(paint: Paint, elementOrigin: Pos): awt.Paint =
    paint match
      case color: Color             => colorToAWT(color)
      case gradient: LinearGradient => linearGradientToAWT(gradient, elementOrigin)

  def strokeToAWT(stroke: StrokeStyle): awt.Stroke =
    val cap = stroke.cap match
      case StrokeStyle.Cap.Butt   => awt.BasicStroke.CAP_BUTT
      case StrokeStyle.Cap.Round  => awt.BasicStroke.CAP_ROUND
      case StrokeStyle.Cap.Square => awt.BasicStroke.CAP_SQUARE

    val join = stroke.join match
      case StrokeStyle.Join.Miter => awt.BasicStroke.JOIN_MITER
      case StrokeStyle.Join.Round => awt.BasicStroke.JOIN_ROUND
      case StrokeStyle.Join.Bevel => awt.BasicStroke.JOIN_BEVEL

    new awt.BasicStroke(
      stroke.width.toFloat,
      cap,
      join,
      // AWT's own default, stated because the boundaries count on every backend using it.
      StrokeStyle.MiterLimit.toFloat
    )

  /** Resolves a font by name, size, and CSS-style weight.
    *
    * AWT offers only PLAIN and BOLD, so every weight snaps to one of them at 700. Weights 100-699
    * render plain and 700-900 bold. That is coarser than the browser, which picks the nearest
    * available face and so renders 500-699 bold for most families, so JVM and JS output differ in
    * that band. `TextAttribute.WEIGHT` does not help. Its named constants quantize at the same
    * threshold, and the platform fonts carry no intermediate faces to select.
    */
  def fontToAWT(font: String, size: Double, weight: Int): awt.Font =
    // deriveFont keeps the fractional point size, which the Int constructor would truncate.
    val style = if weight >= 700 then awt.Font.BOLD else awt.Font.PLAIN
    new awt.Font(FontFamily.awt(font), style, 0).deriveFont(size.toFloat)
