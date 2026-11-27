package smile.infrastructure

import com.sun.net.httpserver.{HttpExchange, HttpServer}
import smile.colors.Color
import smile.modeling.Bounds
import smile.pictures.{AnimatedPicture, Bitmap, SvgPicture, toBitmap}

import java.awt.image.BufferedImage
import java.io.{ByteArrayOutputStream, IOException}
import java.net.InetSocketAddress
import java.nio.charset.StandardCharsets
import java.util.concurrent.ConcurrentLinkedQueue
import javax.imageio.ImageIO
import scala.jdk.CollectionConverters.IteratorHasAsScala

class JVMImageLoadingSuite extends munit.FunSuite:

  private val red  = Color(255, 0, 0)
  private val blue = Color(0, 0, 255)

  private def encoded(format: String): Array[Byte] =
    val image = new BufferedImage(2, 1, BufferedImage.TYPE_INT_ARGB)
    image.setRGB(0, 0, red.toARGBInt)
    image.setRGB(1, 0, blue.toARGBInt)
    val bytes = ByteArrayOutputStream()
    assert(ImageIO.write(image, format, bytes), s"No $format writer available")
    bytes.toByteArray

  private def respond(
      exchange: HttpExchange,
      status: Int,
      headers: Seq[(String, String)],
      body: Array[Byte]
  ): Unit =
    headers.foreach((name, value) => exchange.getResponseHeaders.add(name, value))
    exchange.sendResponseHeaders(status, if body.isEmpty then -1 else body.length.toLong)
    if body.nonEmpty then exchange.getResponseBody.write(body)
    exchange.close()

  /** Runs `test` against a local server on an ephemeral port that redirects `/old.png`, `/old.gif`
    * and `/old.svg` to their images, redirects `/loop` to itself and serves a web page at `/page`.
    * `test` gets the server's base URL and the `User-Agent` of every request it received.
    */
  private def withImageServer[A](test: (String, ConcurrentLinkedQueue[String]) => A): A =
    val userAgents = ConcurrentLinkedQueue[String]()
    val server     = HttpServer.create(InetSocketAddress("127.0.0.1", 0), 0)
    server.createContext(
      "/",
      exchange =>
        Option(exchange.getRequestHeaders.getFirst("User-Agent")).foreach(userAgents.add)
        exchange.getRequestURI.getPath match
          case "/old.png" => respond(exchange, 302, Seq("Location" -> "/images/new.png"), Array())
          case "/old.gif" => respond(exchange, 301, Seq("Location" -> "new.gif"), Array())
          case "/images/new.png" =>
            respond(exchange, 200, Seq("Content-Type" -> "image/png"), encoded("png"))
          case "/new.gif" =>
            respond(exchange, 200, Seq("Content-Type" -> "image/gif"), encoded("gif"))
          case "/old.svg" => respond(exchange, 302, Seq("Location" -> "/images/new.svg"), Array())
          case "/images/new.svg" =>
            val svg = """<svg xmlns="http://www.w3.org/2000/svg" width="4" height="2">
              <rect width="2" height="2" fill="red"/></svg>""".getBytes(StandardCharsets.UTF_8)
            respond(exchange, 200, Seq("Content-Type" -> "image/svg+xml"), svg)
          case "/loop" => respond(exchange, 307, Seq("Location" -> "/loop"), Array())
          case "/page" =>
            val html = "<html>Not an image</html>".getBytes(StandardCharsets.UTF_8)
            respond(exchange, 200, Seq("Content-Type" -> "text/html"), html)
          case _ => respond(exchange, 404, Seq.empty, Array())
    )
    server.start()
    try test(s"http://127.0.0.1:${server.getAddress.getPort}", userAgents)
    finally server.stop(0)

  test("a URL image loads through an HTTP redirect, sending a User-Agent") {
    withImageServer: (base, userAgents) =>
      val bitmap = Bitmap(s"$base/old.png")

      assertEquals(bitmap.pixels().width, 2)
      assertEquals(bitmap.pixels().height, 1)
      assertEquals(bitmap.getColor(0, 0), red)
      assertEquals(bitmap.getColor(1, 0), blue)
      assertEquals(userAgents.iterator.asScala.toVector, Vector("SMILe", "SMILe"))
  }

  test("a URL GIF loads as an animated picture through a relative redirect") {
    withImageServer: (base, _) =>
      val animation = AnimatedPicture.fromGifPath(s"$base/old.gif")

      assertEquals(animation.frames.length, 1)
      animation.frames.head match
        case frame: Bitmap => assertEquals(frame.getColor(0, 0), red)
        case other         => fail(s"Expected a bitmap frame, got $other")
  }

  test("a URL SVG loads as a vector picture through an HTTP redirect") {
    withImageServer: (base, userAgents) =>
      val svg = SvgPicture(s"$base/old.svg")

      assertEquals(svg.bounds, Some(Bounds(-2, -1, 2, 1)))
      assertEquals(svg.toBitmap().getColor(0, 0), red)
      assertEquals(svg.toBitmap().getColor(3, 0).opacity, 0)
      assertEquals(userAgents.iterator.asScala.toVector, Vector("SMILe", "SMILe"))
      assertEquals(
        intercept[IOException](SvgPicture(s"$base/missing.svg")).getMessage,
        s"SVG at path '$base/missing.svg' not found"
      )
  }

  test("a classpath image loads with or without a leading slash") {
    for path <- Seq("images/red-blue.png", "/images/red-blue.png") do
      val bitmap = Bitmap(path)
      assertEquals(bitmap.getColor(0, 0), red, path)
      assertEquals(bitmap.getColor(1, 0), blue, path)
  }

  test("an endless redirect loop fails with an IOException naming the path") {
    withImageServer: (base, _) =>
      val error = intercept[IOException](Bitmap(s"$base/loop"))

      assert(error.getMessage.contains(s"$base/loop"), error.getMessage)
      assert(Option(error.getCause).exists(_.getMessage.contains("Too many redirects")))
  }

  test("a URL serving a web page reports what the server sent") {
    withImageServer: (base, _) =>
      val error = intercept[IOException](Bitmap(s"$base/page"))

      assertEquals(
        error.getMessage,
        s"Image at path '$base/page' could not be decoded (the server sent text/html)"
      )
  }

  test("an empty or blank image path is reported as such") {
    for path <- Seq("", "  ") do
      assertEquals(intercept[IOException](Bitmap(path)).getMessage, s"Image path '$path' is empty")
      assertEquals(
        intercept[IOException](AnimatedPicture.fromGifPath(path)).getMessage,
        s"Image path '$path' is empty"
      )
  }
end JVMImageLoadingSuite
