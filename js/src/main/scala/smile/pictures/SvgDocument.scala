package smile.pictures

import org.scalajs.dom
import smile.infrastructure.Completeness
import smile.modeling.Affine

import java.io.IOException
import scala.collection.mutable
import scala.concurrent.{Future, Promise}
import scala.scalajs.js

/** A parsed static SVG document in the browser, kept as the document's stripped text in a data URL,
  * which every picture of it shares, read from `path` when it has one. The browser starts decoding
  * it when it is made, and canvas output draws it at the output's own density once it is decoded.
  * Before then a rendering leaves it out and says so to `smile.infrastructure.Completeness`, so
  * that nothing keeps that rendering. SVG output embeds the document as an image, which needs no
  * decoding.
  */
private[smile] final class SvgDocument private[pictures] (
    val width: Double,
    val height: Double,
    val dataUrl: String,
    path: Option[String]
) extends SvgDocumentLike:

  /** The decoded image, once `ready` has completed. */
  private var decoded: Option[dom.html.Image] = None

  /** Completes when the document is decoded for canvas drawing, once for every picture of it, and
    * fails if the browser cannot decode it, which then draws nothing. No raster buffer is made
    * here.
    */
  val ready: Future[Unit] =
    val promise = Promise[Unit]()
    val image   = new dom.Image()
    image.addEventListener(
      "load",
      (_: js.Any) =>
        decoded = Some(image)
        promise.success(())
    )
    image.addEventListener(
      "error",
      (_: js.Any) =>
        val at = path.fold("")(read => s" at path '$read'")
        // Nothing else says so unless someone awaits the future ready, and the picture draws
        // nothing.
        dom.console.warn(s"SVG$at could not be decoded by the browser; it draws nothing")
        promise.failure(new IOException(s"SVG$at could not be decoded by the browser"))
    )
    image.src = dataUrl
    promise.future

  /** Draws the document onto `ctx`, with `m` taking the document's own pixels to the coordinates
    * `ctx` draws in, once it is decoded. While it is decoding it draws nothing and tells
    * `smile.infrastructure.Completeness` that the rendering left it out. Once decoding has failed,
    * which is final, it draws nothing and the rendering is complete.
    */
  def drawTo(ctx: dom.CanvasRenderingContext2D, m: Affine): Unit =
    decoded match
      case None        => if !ready.isCompleted then Completeness.omitted()
      case Some(image) =>
        ctx.save()
        try
          ctx.transform(m.a, m.b, m.c, m.d, m.e, m.f)
          ctx.drawImage(image, 0, 0, width, height)
        finally ctx.restore()
end SvgDocument

private[smile] object SvgDocument:
  /** Parses `svg`, a self-contained static SVG document, synchronously, by the rules of
    * `smile.pictures.SvgReading`. What is not part of the static picture is removed and the size is
    * resolved from the root's attributes. The browser loads nothing outside the document, and draws
    * PNG, JPEG and GIF rasters embedded in it as `data:` URLs. A `switch` keeps its first child.
    * Decoding starts here. Await `ready` before reading pixels.
    *
    * @throws java.io.IOException
    *   if the text is not well-formed XML with an SVG root, its elements nest too deep, or its size
    *   or viewBox is invalid or over the maximum side
    */
  def fromString(svg: String): SvgDocument =
    SvgReading.reading(None)(read(svg, None))

  /** Parses `content`, the text read at `path`, as `fromString` parses a text.
    *
    * @throws java.io.IOException
    *   naming `path`, if `content` cannot be read as `fromString` reads
    */
  def fromContent(path: String, content: String): SvgDocument =
    SvgReading.reading(Some(path))(read(content, Some(path)))

  /** The stripped text the browser decodes for `svg`, with its size in pixels on the root. */
  private[pictures] def normalized(svg: String): String = normalizedText(svg)(0)

  /** Reads a document, with the reason in the message of the exception if it cannot. */
  private def read(svg: String, path: Option[String]): SvgDocument =
    val (text, width, height) = normalizedText(svg)
    val dataUrl = "data:image/svg+xml;charset=utf-8," + js.URIUtils.encodeURIComponent(text)
    new SvgDocument(width, height, dataUrl, path)

  /** The text of the document stripped and sized by `normalizedRoot`, with its size in pixels. */
  private def normalizedText(svg: String): (String, Double, Double) =
    val (root, width, height) = normalizedRoot(svg)
    (new dom.XMLSerializer().serializeToString(root), width, height)

  /** Parses, strips and sizes the document, and writes its size on the root in pixels. */
  private def normalizedRoot(svg: String): (dom.Element, Double, Double) =
    val document = new dom.DOMParser().parseFromString(svg, dom.MIMEType.`image/svg+xml`)
    val errors   = document.getElementsByTagName("parsererror")
    if errors.length != 0 then
      // Chromium writes the parser's message in a div element, among its own headings.
      val error = Option(errors(0).querySelector("div")).getOrElse(errors(0))
      throw new IOException(SvgReading.notWellFormed(error.textContent.trim))
    val root = document.documentElement
    if root.namespaceURI != SvgReading.Namespace || root.localName != "svg" then
      throw new IOException(SvgReading.rootIsNotSvg(root.tagName))
    requireDepth(root)
    strip(root)
    def attribute(name: String) = Option.when(root.hasAttribute(name))(root.getAttribute(name))
    val (width, height)         =
      SvgReading.size(attribute("width"), attribute("height"), attribute("viewBox"))
    root.setAttribute("width", width.toString)
    root.setAttribute("height", height.toString)
    (root, width, height)

  /** Refuses a document whose elements nest deeper than `SvgReading.MaxDepth` below `root`, before
    * the recursive walk, without recursion.
    */
  private def requireDepth(root: dom.Element): Unit =
    val pending = mutable.Stack(root -> 0)
    while pending.nonEmpty do
      val (element, depth) = pending.pop()
      if depth > SvgReading.MaxDepth then throw new IOException(SvgReading.nestsTooDeep)
      for index <- 0 until element.children.length do
        pending.push(element.children(index) -> (depth + 1))

  /** Removes from `element`'s subtree, and from its own attributes, what `SvgReading` refuses, and
    * every processing instruction, and keeps only the first child of a `switch`, which, with its
    * conditions stripped, then draws that child.
    */
  private def strip(element: dom.Element): Unit =
    val attributes = element.attributes
    for index <- (attributes.length - 1) to 0 by -1 do
      val attribute = attributes(index)
      if !SvgReading.keepsAttribute(
          element.localName,
          attribute.namespaceURI,
          attribute.localName,
          attribute.value
        )
      then
        val _ = element.removeAttributeNode(attribute)
    val children = element.childNodes
    for index <- (children.length - 1) to 0 by -1 do
      children(index) match
        case child: dom.Element =>
          if SvgReading.keepsElement(child.namespaceURI, child.localName) then strip(child)
          else
            val _ = element.removeChild(child)
        case node if node.nodeType == dom.Node.PROCESSING_INSTRUCTION_NODE =>
          val _ = element.removeChild(node)
        case _ => ()
    if SvgReading.keepsFirstChildOnly(element.namespaceURI, element.localName) then
      val first = Option(element.firstElementChild)
      (0 until children.length)
        .map(children(_))
        .filterNot(node => first.exists(_ eq node))
        .foreach(node => element.removeChild(node))
end SvgDocument
