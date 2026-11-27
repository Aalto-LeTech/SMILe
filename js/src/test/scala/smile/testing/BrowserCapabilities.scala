package smile.testing

import org.scalajs.dom

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.scalajs.js
import scala.util.Try

/** Capability probes for the JS environment the tests happen to run in.
  *
  * The Scala.js suites run under jsdom by default and in a real headless browser when the build is
  * started with `-Dsmile.browser=true`, which build.sbt reads. Each probe asks whether the DOM
  * capability a test needs works, so a test runs wherever it can and is skipped where it cannot.
  */
object BrowserCapabilities:

  /** Whether the environment lays out SVG text, i.e. whether `SVGElement.getBBox` returns a real
    * box. jsdom has no layout engine and does not implement the method at all. Every real browser
    * does, which makes this the practical "is this a full browser engine?" probe as well.
    */
  lazy val laysOutSVGText: Boolean =
    Try {
      val svgNamespace = "http://www.w3.org/2000/svg"
      val svg          = dom.document.createElementNS(svgNamespace, "svg")
      val text         = dom.document.createElementNS(svgNamespace, "text")
      text.textContent = "probe"
      val _ = svg.appendChild(text)
      val _ = dom.document.body.appendChild(svg)
      // getBBox throws where it is not implemented, which is the answer this probe is after. The
      // probe node still has to go, or the failed probe would leave it in the document.
      val box =
        try svg.asInstanceOf[dom.SVGLocatable].getBBox()
        finally
          val _ = dom.document.body.removeChild(svg)
      box.width > 0
    }.getOrElse(false)

  /** Whether the engine is WebKit itself, as in Safari or Playwright's build of it. Chromium and
    * jsdom name AppleWebKit in their user agents too, next to their own names.
    */
  lazy val isWebKit: Boolean =
    val agent = dom.window.navigator.userAgent
    agent.contains("AppleWebKit") && !agent.contains("Chrome") && !agent.contains("jsdom")

  lazy val isChromium: Boolean = dom.window.navigator.userAgent.contains("Chrome")

  /** Whether the environment provides WebCodecs' `ImageDecoder`, the API real GIF frame extraction
    * goes through. Chromium ships it. WebKit, Firefox and jsdom do not, and there they fall back to
    * a single-frame animation.
    */
  lazy val extractsImageFrames: Boolean =
    js.typeOf(js.Dynamic.global.ImageDecoder) != "undefined"

  /** Whether `HTMLImageElement` decodes an image, probed by loading a 1x1 data URL and seeing
    * whether the `load` event arrives with the right dimensions. Environments that never answer
    * either way are treated as incapable after `ProbeTimeout`.
    */
  def decodesImages(using ExecutionContext): Future[Boolean] =
    decodeProbe match
      case Some(pending) => pending
      case None          =>
        val probe = attemptDecode(OnePixelPNG).map(_.contains((1, 1))).recover(_ => false)
        decodeProbe = Some(probe)
        probe

  private val ProbeTimeout = 5000

  private var decodeProbe: Option[Future[Boolean]] = None

  /** An opaque black 1x1 PNG. */
  private val OnePixelPNG =
    "data:image/png;base64,iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAIAAACQd1PeAAAADElEQVR42mNgYGAAAAAEAAHI" +
      "6uv5AAAAAElFTkSuQmCC"

  private def attemptDecode(source: String): Future[Option[(Int, Int)]] =
    val outcome = Promise[Option[(Int, Int)]]()
    val image   = new dom.Image()
    image.addEventListener(
      "load",
      (_: js.Any) => outcome.trySuccess(Some((image.width, image.height)))
    )
    image.addEventListener("error", (_: js.Any) => outcome.trySuccess(None))
    val _ = dom.window.setTimeout(() => outcome.trySuccess(None), ProbeTimeout)
    image.src = source
    outcome.future
