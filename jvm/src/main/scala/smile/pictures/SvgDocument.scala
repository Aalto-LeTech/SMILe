package smile.pictures

import com.github.weisj.jsvg.SVGDocument
import com.github.weisj.jsvg.parser.{DocumentLimits, LoaderContext, SVGLoader}
import com.github.weisj.jsvg.parser.resources.ResourcePolicy
import org.w3c.dom.{Attr, Document, Element, Node, ProcessingInstruction}
import org.xml.sax.{ErrorHandler, InputSource, SAXException, SAXParseException}
import smile.modeling.Affine

import java.awt.Graphics2D
import java.awt.geom.{AffineTransform, Rectangle2D}
import java.io.{ByteArrayInputStream, ByteArrayOutputStream, IOException, StringReader}
import java.math.BigDecimal as JBigDecimal
import java.nio.charset.StandardCharsets.UTF_8
import javax.xml.XMLConstants
import javax.xml.parsers.DocumentBuilderFactory
import javax.xml.transform.{OutputKeys, TransformerFactory}
import javax.xml.transform.dom.DOMSource
import javax.xml.transform.stream.StreamResult
import scala.collection.mutable
import scala.util.control.NonFatal

/** A parsed static SVG document on the JVM, which every picture of it shares with its renderer
  * caches, and draws as vectors at the output's own density.
  */
private[smile] final class SvgDocument private (
    document: SVGDocument,
    val width: Double,
    val height: Double
) extends SvgDocumentLike:

  /** Draws the document onto `graphics`, with `m` taking the document's own pixels to the
    * coordinates `graphics` draws in, and nothing outside the document's box.
    */
  def render(graphics: Graphics2D, m: Affine): Unit =
    val local = graphics.create().asInstanceOf[Graphics2D]
    try
      local.transform(new AffineTransform(m.a, m.b, m.c, m.d, m.e, m.f))
      local.clip(new Rectangle2D.Double(0, 0, width, height))
      // Every picture of the document shares it, including its renderer caches.
      document.synchronized:
        document.render(null, local)
    finally local.dispose()
end SvgDocument

private[smile] object SvgDocument:

  /** Parses `svg`, a self-contained static SVG document, by the rules of
    * `smile.pictures.SvgReading`. What is not part of the static picture is removed, the size is
    * resolved from the root's attributes, and nothing outside the document is read or fetched. PNG,
    * JPEG and GIF rasters embedded in it as `data:` URLs are decoded and drawn. Internal DTD
    * entities are expanded, such as the ones Illustrator writes. External entities and an external
    * DTD are not read. A `switch` draws its first child, as a group, since JSVG draws no `switch`.
    *
    * @throws java.io.IOException
    *   if the text is not well-formed XML with an SVG root, its size or viewBox is invalid or over
    *   the maximum side, or JSVG cannot load it
    */
  def fromString(svg: String): SvgDocument =
    SvgReading.reading(None)(read(new InputSource(new StringReader(svg))))

  /** Parses `content`, what was read at `path`, as `fromString` parses a text, with the XML's own
    * encoding.
    *
    * @throws java.io.IOException
    *   naming `path`, if `content` cannot be read as `fromString` reads
    */
  def fromContent(path: String, content: Array[Byte]): SvgDocument =
    SvgReading.reading(Some(path)):
      read(new InputSource(new ByteArrayInputStream(content)))

  /** The most elements JSVG draws, counting each `use` of one. */
  private val MaxElements = 2000

  /** The deepest that `use` elements may refer to ones that refer to others. */
  private val MaxUseDepth = 15

  /** The stripped text JSVG reads for `svg`, with its size in pixels on the root. */
  private[pictures] def normalized(svg: String): String =
    val root = normalizedRoot(new InputSource(new StringReader(svg)))
    new String(serialized(root.element), UTF_8)

  /** A root element as JSVG gets it, with its size in pixels. */
  private final case class Root(element: Element, width: Double, height: Double)

  /** Reads a document, with the reason in the message of the exception if it cannot. */
  private def read(source: InputSource): SvgDocument =
    val root    = normalizedRoot(source)
    val context = LoaderContext
      .builder()
      .externalResourcePolicy(ResourcePolicy.DENY_EXTERNAL)
      .documentLimits(new DocumentLimits(SvgReading.MaxDepth, MaxUseDepth, MaxElements))
      .build()
    val document =
      try new SVGLoader().load(new ByteArrayInputStream(serialized(root.element)), null, context)
      catch
        case NonFatal(problem) =>
          throw new IOException(s"JSVG could not load it (${problem.getMessage})", problem)
    // JSVG says why it could not load a document only to its logger, so name what refuses one.
    if document == null then
      throw new IOException(
        s"JSVG could not load it (it may be over the limits of $MaxElements drawn elements, " +
          s"a nesting depth of ${SvgReading.MaxDepth} or a use depth of $MaxUseDepth)"
      )
    new SvgDocument(document, root.width, root.height)

  /** Parses, strips and sizes the document, and writes its size on the root in pixels. */
  private def normalizedRoot(source: InputSource): Root =
    val root = parsed(source).getDocumentElement
    if root.getNamespaceURI != SvgReading.Namespace || root.getLocalName != "svg" then
      throw new IOException(SvgReading.rootIsNotSvg(root.getTagName))
    requireDepth(root)
    strip(root)
    def attribute(name: String) = Option.when(root.hasAttribute(name))(root.getAttribute(name))
    val (width, height)         =
      SvgReading.size(attribute("width"), attribute("height"), attribute("viewBox"))
    root.setAttribute("width", JBigDecimal.valueOf(width).toPlainString)
    root.setAttribute("height", JBigDecimal.valueOf(height).toPlainString)
    Root(root, width, height)

  /** Parses XML with its internal entities expanded, and nothing external read or fetched. */
  private def parsed(source: InputSource): Document =
    val factory = DocumentBuilderFactory.newInstance()
    factory.setNamespaceAware(true)
    factory.setXIncludeAware(false)
    factory.setExpandEntityReferences(true)
    factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    factory.setFeature("http://xml.org/sax/features/external-general-entities", false)
    factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false)
    factory.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false)
    val builder = factory.newDocumentBuilder()
    builder.setErrorHandler(new ErrorHandler:
      def warning(exception: SAXParseException): Unit    = ()
      def error(exception: SAXParseException): Unit      = throw exception
      def fatalError(exception: SAXParseException): Unit = throw exception)
    try builder.parse(source)
    catch
      case problem: SAXException =>
        throw new IOException(SvgReading.notWellFormed(problem.getMessage), problem)

  /** Refuses a document whose elements nest deeper than `SvgReading.MaxDepth` below `root`, before
    * the recursive walks, without recursion.
    */
  private def requireDepth(root: Element): Unit =
    val pending = mutable.Stack((root: Node) -> 0)
    while pending.nonEmpty do
      val (node, depth) = pending.pop()
      if depth > SvgReading.MaxDepth then throw new IOException(SvgReading.nestsTooDeep)
      val children = node.getChildNodes
      for index <- 0 until children.getLength do
        if children.item(index).getNodeType == Node.ELEMENT_NODE then
          pending.push(children.item(index) -> (depth + 1))

  /** Removes from `element`'s subtree, and from its own attributes, what `SvgReading` refuses, and
    * every processing instruction, and draws a `switch` as a group of its first child.
    */
  private def strip(element: Element): Unit =
    val attributes = element.getAttributes
    for index <- (attributes.getLength - 1) to 0 by -1 do
      val attribute = attributes.item(index).asInstanceOf[Attr]
      val name      = Option(attribute.getLocalName).getOrElse(attribute.getName)
      if !SvgReading.keepsAttribute(
          element.getLocalName,
          attribute.getNamespaceURI,
          name,
          attribute.getValue
        )
      then
        val _ = element.removeAttributeNode(attribute)
    val children = element.getChildNodes
    for index <- (children.getLength - 1) to 0 by -1 do
      children.item(index) match
        case child: Element =>
          if SvgReading.keepsElement(child.getNamespaceURI, child.getLocalName) then strip(child)
          else
            val _ = element.removeChild(child)
        case instruction: ProcessingInstruction =>
          val _ = element.removeChild(instruction)
        case _ => ()
    if SvgReading.keepsFirstChildOnly(element.getNamespaceURI, element.getLocalName) then
      val nodes = (0 until children.getLength).map(children.item)
      val first = nodes.collectFirst { case child: Element => child }
      nodes.filterNot(first.contains).foreach(node => element.removeChild(node))
      // JSVG draws no switch element. A group of its one child draws the same.
      val name = Option(element.getPrefix).fold("g")(prefix => s"$prefix:g")
      val _    = element.getOwnerDocument.renameNode(element, SvgReading.Namespace, name)

  /** The element as UTF-8 XML without a declaration, and without the document's DOCTYPE. */
  private def serialized(element: Element): Array[Byte] =
    val factory = TransformerFactory.newInstance()
    factory.setFeature(XMLConstants.FEATURE_SECURE_PROCESSING, true)
    val transformer = factory.newTransformer()
    transformer.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    transformer.setOutputProperty(OutputKeys.ENCODING, "UTF-8")
    val bytes = new ByteArrayOutputStream()
    transformer.transform(new DOMSource(element), new StreamResult(bytes))
    bytes.toByteArray
end SvgDocument
