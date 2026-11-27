package smile.pictures

import java.io.IOException
import scala.util.control.NonFatal

/** The rules every platform reads an SVG document by, so that a file is the same picture on each.
  * They give its size, from the root's attributes, and which of its elements and attributes are
  * kept. Each platform parses the document into its own DOM, removes what `keepsElement` and
  * `keepsAttribute` refuse and every processing instruction, keeps only the first remaining element
  * child of an element that `keepsFirstChildOnly`, sets the root's `width` and `height` to `size`
  * in pixels, and hands the result to its renderer.
  *
  * What is kept is the static, self-contained picture in the document. Scripts, event attributes,
  * animation elements, conditions and foreign content are removed, and so are references outside
  * the document except embedded PNG, JPEG and GIF rasters. CSS is left to the renderers. `url(...)`
  * and `@import` in `style` elements, `style` attributes and presentation attributes stay, because
  * neither renderer loads anything external for a document. JSVG denies external resources, and a
  * browser loads none for an image. Reading a document relies on that for security. CSS animations
  * and transitions stay too, and never change a picture over time. A browser draws an SVG image
  * onto a canvas as it is at the start of its animations, and JSVG ignores them, so a picture that
  * uses them can differ between the platforms.
  *
  * The messages of the exceptions here are reasons, which the loaders put after `SVG could not be
  * read: ` or `SVG at path '<path>' could not be read: `.
  */
private[smile] object SvgReading:

  /** The SVG namespace, which the root element and every kept element are in. */
  val Namespace = "http://www.w3.org/2000/svg"

  /** The largest width or height, in pixels, that a document may have. */
  val MaxSide = 16384

  /** The deepest that elements may nest below the root. */
  val MaxDepth = 30

  private val XLinkNamespace = "http://www.w3.org/1999/xlink"
  private val XmlNamespace   = "http://www.w3.org/XML/1998/namespace"
  private val XmlNsNamespace = "http://www.w3.org/2000/xmlns/"

  /** A number as SVG writes one, the same on every platform, which `toDouble` then reads. */
  private val NumberSyntax = """[+-]?(?:\d+(?:\.\d*)?|\.\d+)(?:[eE][+-]?\d+)?"""
  private val Length       = s"($NumberSyntax)([a-z%]*)".r
  private val ViewBoxValue = NumberSyntax.r

  /** Pixels per unit, at 96 pixels per inch and a 16 pixel font. */
  private val PixelsPerUnit = Map(
    ""   -> 1.0,
    "px" -> 1.0,
    "pt" -> 96.0 / 72,
    "pc" -> 16.0,
    "mm" -> 96 / 25.4,
    "cm" -> 96 / 2.54,
    "in" -> 96.0,
    "em" -> 16.0,
    "ex" -> 8.0
  )

  /** The conditional processing attributes, which are stripped, so that a `switch` draws its first
    * child.
    */
  private val Conditions = Set("requiredfeatures", "requiredextensions", "systemlanguage")

  /** The media types of the embedded rasters that are kept, which every renderer decodes. */
  private val RasterTypes = Set("image/png", "image/jpeg", "image/gif")

  /** Elements of the SVG namespace that are stripped, in lower case. */
  private val StrippedElements = Set(
    "metadata",
    "script",
    "foreignobject",
    "animate",
    "animatetransform",
    "animatemotion",
    "animatecolor",
    "set",
    "discard"
  )

  /** The document's size in pixels from its root's `width`, `height` and `viewBox` attributes,
    * where an absent or blank attribute is `None`.
    *
    * A side in an absolute unit is that length at 96 pixels per inch. The absolute units are none,
    * `px`, `pt`, `pc`, `mm`, `cm` and `in`. A font-relative side counts `em` as 16 pixels and `ex`
    * as 8. A side that is missing or a percentage is unspecified. With the other side given, it
    * keeps the viewBox's ratio. With neither given, the size is the viewBox's. Without a viewBox,
    * an unspecified width is 300 and an unspecified height 150.
    *
    * @throws java.io.IOException
    *   if a side is not a positive length, the viewBox is not four numbers with a positive width
    *   and height, or a side of the size is over `MaxSide`
    */
  def size(
      width: Option[String],
      height: Option[String],
      viewBox: Option[String]
  ): (Double, Double) =
    val box    = viewBox.filterNot(_.trim.isEmpty).map(viewBoxSize)
    val (w, h) = (length("width", width), length("height", height), box) match
      case (Some(w), Some(h), _)               => (w, h)
      case (Some(w), None, Some((boxW, boxH))) => (w, w * boxH / boxW)
      case (Some(w), None, None)               => (w, 150.0)
      case (None, Some(h), Some((boxW, boxH))) => (h * boxW / boxH, h)
      case (None, Some(h), None)               => (300.0, h)
      case (None, None, Some(boxSize))         => boxSize
      case (None, None, None)                  => (300.0, 150.0)
    if !(w <= MaxSide && h <= MaxSide) then
      throw new IOException(
        s"it is ${pixels(w)} x ${pixels(h)} pixels, and a side may be at most $MaxSide"
      )
    (w, h)

  /** Whether an element, by its namespace (`null` or empty for none) and local name, is kept. */
  def keepsElement(namespace: String, localName: String): Boolean =
    namespace == Namespace && !StrippedElements.contains(localName.toLowerCase)

  /** Whether only the first element child that remains of an element, by its namespace and local
    * name, is kept. That holds for a `switch`, which draws its first child whose conditions hold,
    * and every child's conditions are stripped.
    */
  def keepsFirstChildOnly(namespace: String, localName: String): Boolean =
    namespace == Namespace && localName.toLowerCase == "switch"

  /** Whether an attribute of a kept element, by that element's local name and the attribute's
    * namespace (`null` or empty for none), local name and value, is kept.
    */
  def keepsAttribute(
      element: String,
      namespace: String,
      localName: String,
      value: String
  ): Boolean =
    val ns = Option(namespace).getOrElse("")
    if ns == XmlNsNamespace then true
    else if localName.toLowerCase.startsWith("on") then false
    else if ns == XmlNamespace then localName != "base"
    else if ns != "" && ns != XLinkNamespace then false
    else if ns == "" && Conditions.contains(localName.toLowerCase) then false
    else if localName == "href" then keepsReference(element, value.trim)
    else true

  /** `read`, a platform's reading of a document, or an `IOException` that says the document, at
    * `path` when it was read from one, could not be read, and gives the reason, the same on every
    * platform. The message is `SVG could not be read: <reason>` or `SVG at path '<path>' could not
    * be read: <reason>`.
    */
  def reading[A](path: Option[String])(read: => A): A =
    val failure =
      path.fold("SVG could not be read")(path => s"SVG at path '$path' could not be read")
    try read
    catch
      case reason: IOException => throw new IOException(s"$failure: ${reason.getMessage}", reason)
      case NonFatal(problem)   => throw new IOException(s"$failure: $problem", problem)

  /** The reason for a document that the parser refuses, with the parser's own message. */
  def notWellFormed(parserMessage: String): String =
    s"it is not well-formed XML ($parserMessage)"

  /** The reason for a document whose root, of qualified name `name`, is not an SVG `svg`. */
  def rootIsNotSvg(name: String): String = s"its root element is '$name', not an SVG 'svg'"

  /** The reason for a document whose elements nest deeper than `MaxDepth` below the root. */
  val nestsTooDeep: String = s"its elements nest more than $MaxDepth deep"

  /** A reference within the document, or an embedded PNG, JPEG or GIF raster for an image. */
  private def keepsReference(element: String, url: String): Boolean =
    def mediaType = url.toLowerCase.stripPrefix("data:").takeWhile(c => c != ';' && c != ',')
    url.startsWith("#") ||
    (Set("image", "feimage").contains(element.toLowerCase) &&
      url.toLowerCase.startsWith("data:") && RasterTypes.contains(mediaType.trim))

  /** A side in pixels, or `None` where it is unspecified, as it is when empty, a percentage or
    * `auto`.
    */
  private def length(side: String, value: Option[String]): Option[Double] =
    value
      .map(_.trim)
      .filter(_.nonEmpty)
      .flatMap: text =>
        def refused = new IOException(s"its $side '$text' is not a positive length")
        text.toLowerCase match
          case Length(number, unit) if unit == "%" || PixelsPerUnit.contains(unit) =>
            val inPixels = number.toDouble * PixelsPerUnit.getOrElse(unit, 1.0)
            if !(inPixels > 0 && inPixels.isFinite) then throw refused
            Option.when(unit != "%")(inPixels)
          case "auto" => None
          case _      => throw refused

  private def viewBoxSize(viewBox: String): (Double, Double) =
    val numbers = viewBox.trim
      .split("[\\s,]+")
      .toSeq
      .map: value =>
        Option.when(ViewBoxValue.matches(value))(value.toDouble).filter(_.isFinite)
    numbers match
      case Seq(Some(_), Some(_), Some(w), Some(h)) if w > 0 && h > 0 => (w, h)
      case _                                                         =>
        throw new IOException(
          s"its viewBox '$viewBox' is not four numbers with a positive width and height"
        )

  /** A number of pixels as the messages show it, to two decimals, the same on every platform up to
    * a trillion.
    */
  private def pixels(value: Double): String =
    if !(value < 1e12) then value.toString
    else
      val rounded = math.round(value * 100) / 100.0
      if rounded == rounded.floor then rounded.toLong.toString else rounded.toString
end SvgReading
