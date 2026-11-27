package smile.pictures

import org.scalajs.dom
import smile.colors.Color
import smile.infrastructure.Completeness
import smile.modeling.{Affine, Bounds, Pos}
import smile.pictures.FillStyle

import java.io.IOException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/** SVG pictures as elements in the browser, the shared class over the browser's document. */
class JSSvgElementSuite extends munit.FunSuite:

  private val source =
    """<svg xmlns="http://www.w3.org/2000/svg" width="20" height="10" viewBox="0 0 20 10"><path fill="red" d="M0 0H10V10H0Z"/></svg>"""

  test("an SVG picture is centered at the origin and scales as its box under the affine") {
    val svg = SvgPicture.fromString(source)
    assertEquals(svg.bounds, Some(Bounds(-10, -5, 10, 5)))
    assertEquals(svg.anchor, Pos.Origin)
    val scaled = svg.scaleBy(2, 1) match
      case s: SvgPicture => s
      case other         => fail(s"$other is not an SVG picture")
    assert(scaled.document eq svg.document)
    assertEquals(scaled.bounds, Some(Bounds(-20, -5, 20, 5)))
    assertEquals(scaled.rotateBy(90).bounds, Some(Bounds(-5, -20, 5, 20)))
  }

  test("an SVG picture masks content as any element does") {
    val svg     = SvgPicture.fromString(source)
    val content = Rectangle(40, 40, Pos.Origin, Some(FillStyle(Color(0, 0, 255))), None)
    MaskGroup(svg, content) match
      case mask: MaskGroup => assertEquals(mask.bounds, svg.bounds)
      case other           => fail(s"$other is not a mask")
  }

  test("SVG pictures are equal by their document, transform and anchor") {
    val svg = SvgPicture.fromString(source)
    assertEquals(svg.rotateBy(90).rotateBy(-90), svg)
    assertNotEquals(svg, SvgPicture.fromString(source))
  }

  test("loading stays asynchronous and ready is an extension in the browser") {
    val svg                                = SvgPicture.fromString(source)
    val prepared: Future[SvgPicture]       = svg.ready
    val load: String => Future[SvgPicture] = SvgPicture.load
    val cached: String => SvgPicture       = SvgPicture(_)
    val _                                  = (prepared, load, cached)
    intercept[IOException](SvgPicture.fromString("<not-svg/>"))
  }

  test(
    "a document the browser cannot decode fails ready, naming its path, and renderings of it are complete"
  ) {
    // jsdom ends the whole run on an image it cannot decode.
    assume(!dom.window.navigator.userAgent.contains("jsdom"), "needs a browser that survives it")
    val document = new SvgDocument(2, 2, "data:image/svg+xml,%3Csvg", None)
    val read     = new SvgDocument(2, 2, "data:image/svg+xml,%3Csvg", Some("broken.svg"))
    val context  =
      dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas].getContext("2d")
    for
      failure       <- document.ready.failed
      failureOfRead <- read.ready.failed
    yield
      assertEquals(failure.getMessage, "SVG could not be decoded by the browser")
      assertEquals(
        failureOfRead.getMessage,
        "SVG at path 'broken.svg' could not be decoded by the browser"
      )
      val (_, whole) = Completeness.tracked:
        document.drawTo(context.asInstanceOf[dom.CanvasRenderingContext2D], Affine.Identity)
      assert(whole, "a failed document is counted as still decoding")
  }

  test("a CSS animation in a style element draws as at its start, whenever it is drawn") {
    // jsdom's canvas draws an SVG image without its CSS animations, so the rectangle stays red.
    assume(!dom.window.navigator.userAgent.contains("jsdom"), "needs a browser's SVG animations")
    val document = SvgDocument.fromString(
      """<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10"><style>
      @keyframes k { from { fill: blue } to { fill: lime } } rect { animation: k 400ms linear }
      </style><rect width="10" height="10" fill="red"/></svg>"""
    )
    def drawn: Int =
      val canvas = dom.document.createElement("canvas").asInstanceOf[dom.html.Canvas]
      canvas.width = 10
      canvas.height = 10
      val context = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
      document.drawTo(context, Affine.Identity)
      val p = context.getImageData(5, 5, 1, 1).data
      (p(3) << 24) | (p(0) << 16) | (p(1) << 8) | p(2)
    for
      _ <- document.ready
      first = drawn
      _ <- later(200)
    yield
      assertEquals(first, 0xff0000ff, "the animation's first frame")
      assertEquals(drawn, first, "the animation ran on")
  }

  /** Completes after `ms` milliseconds. */
  private def later(ms: Int): Future[Unit] =
    val done = scala.concurrent.Promise[Unit]()
    val _    = scala.scalajs.js.timers.setTimeout(ms.toDouble)(done.success(()))
    done.future
end JSSvgElementSuite
