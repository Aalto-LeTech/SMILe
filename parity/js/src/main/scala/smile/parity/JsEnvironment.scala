package smile.parity

import org.scalajs.dom
import org.scalajs.dom.html

import scala.util.Try

/** What the harness asks of the JS environment it runs in. Capabilities are answered by trying
  * them.
  */
object JsEnvironment:

  /** Whether the environment has a font of this family name. A text set in it, with a generic
    * fallback behind it, is as wide as the fallback alone only if the font is missing. Two
    * fallbacks are tried, since the font may itself be what one of them resolves to.
    */
  def hasFont(family: String): Boolean =
    Try {
      val context = dom.document
        .createElement("canvas")
        .asInstanceOf[html.Canvas]
        .getContext("2d")
        .asInstanceOf[dom.CanvasRenderingContext2D]
      def widthIn(font: String): Double =
        context.font = s"72px $font"
        context.measureText("mmmmiiiilllWWW@#gjy").width
      Seq("monospace", "serif").exists: fallback =>
        widthIn(s""""$family", $fallback""") != widthIn(fallback)
    }.getOrElse(false)

  /** The first of [[smile.parity.ParityCases.FontCandidates]] this environment has. */
  lazy val commonFont: Option[String] = ParityCases.FontCandidates.find(hasFont)
