package smile.pictures

import smile.modeling.Bounds

import java.io.IOException

/** Every platform keeps the static, self-contained picture in an SVG document. Everything else is
  * removed before the document is drawn, and the document still loads.
  */
class SvgSanitizingSuite extends munit.FunSuite:

  private val Svg   = SvgReading.Namespace
  private val XLink = "http://www.w3.org/1999/xlink"
  private val Xml   = "http://www.w3.org/XML/1998/namespace"
  private val XmlNs = "http://www.w3.org/2000/xmlns/"

  test("SVG drawing elements are kept, including style, title, desc, switch, a and image") {
    for name <- Seq(
        "svg",
        "g",
        "path",
        "rect",
        "use",
        "defs",
        "linearGradient",
        "clipPath",
        "mask",
        "filter",
        "text",
        "style",
        "title",
        "desc",
        "switch",
        "a",
        "image",
        "feImage"
      )
    do assert(SvgReading.keepsElement(Svg, name), name)
  }

  test("elements outside the SVG namespace, metadata and active elements are stripped") {
    for (namespace, name) <- Seq(
        "http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd" -> "namedview",
        "http://www.inkscape.org/namespaces/inkscape"        -> "page",
        "http://www.w3.org/1999/02/22-rdf-syntax-ns#"        -> "RDF",
        "http://ns.adobe.com/AdobeIllustrator/10.0/"         -> "pgfRef",
        "http://www.w3.org/1999/xhtml"                       -> "div",
        ""                                                   -> "rect",
        null                                                 -> "rect",
        Svg                                                  -> "metadata",
        Svg                                                  -> "script",
        Svg                                                  -> "foreignObject",
        Svg                                                  -> "animate",
        Svg                                                  -> "animateTransform",
        Svg                                                  -> "animateMotion",
        Svg                                                  -> "animateColor",
        Svg                                                  -> "set",
        Svg                                                  -> "discard",
        Svg                                                  -> "SCRIPT"
      )
    do assert(!SvgReading.keepsElement(namespace, name), s"$namespace $name")
  }

  test("ordinary SVG attributes are kept, including style, classes and namespace declarations") {
    for (namespace, name, value) <- Seq(
        (null, "width", "10mm"),
        ("", "d", "M0 0h10"),
        (null, "style", "fill: url(#g)"),
        (null, "class", "st0"),
        (null, "fill", "url(#gradient)"),
        (null, "id", "one"),
        (Xml, "space", "preserve"),
        (XLink, "title", "a title"),
        (XmlNs, "xmlns", Svg),
        (XmlNs, "inkscape", "http://www.inkscape.org/namespaces/inkscape"),
        (XmlNs, "onion", "http://example.com/onion")
      )
    do assert(SvgReading.keepsAttribute("rect", namespace, name, value), s"$namespace $name")
  }

  test("event attributes, xml:base and foreign namespaced attributes are stripped") {
    for (namespace, name, value) <- Seq(
        (null, "onload", "alert(1)"),
        ("", "onclick", "alert(1)"),
        (null, "onMouseOver", "alert(1)"),
        (Xml, "base", "http://example.com/"),
        ("http://www.inkscape.org/namespaces/inkscape", "label", "Layer 1"),
        ("http://www.affinity.serif.com", "id", "head background"),
        ("http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd", "nodetypes", "cc")
      )
    do assert(!SvgReading.keepsAttribute("rect", namespace, name, value), s"$namespace $name")
  }

  test("conditions are stripped, and a switch keeps only its first remaining child") {
    for name <- Seq("requiredFeatures", "requiredExtensions", "systemLanguage") do
      assert(!SvgReading.keepsAttribute("g", null, name, "en"), name)
    assert(SvgReading.keepsFirstChildOnly(Svg, "switch"))
    for (namespace, name) <- Seq(Svg -> "g", Svg -> "svg", "urn:x" -> "switch", null -> "switch")
    do assert(!SvgReading.keepsFirstChildOnly(namespace, name), s"$namespace $name")
  }

  test("references stay only within the document or to an embedded PNG, JPEG or GIF") {
    val png = "data:image/png;base64,iVBORw0KGgo="
    for (element, namespace, value) <- Seq(
        ("use", null, "#shape"),
        ("use", XLink, " #shape"),
        ("image", null, png),
        ("image", XLink, " DATA:IMAGE/png;base64,AAAA"),
        ("feImage", XLink, png),
        ("image", null, "data:image/jpeg;base64,/9j/"),
        ("image", null, "data:image/gif,GIF89a"),
        ("a", null, "#top")
      )
    do assert(SvgReading.keepsAttribute(element, namespace, "href", value), s"$element $value")
    for (element, namespace, value) <- Seq(
        ("use", null, "shapes.svg#shape"),
        ("use", XLink, "http://example.com/shapes.svg#shape"),
        ("image", null, "picture.png"),
        ("image", XLink, "file:///etc/passwd"),
        ("image", null, "data:text/html,<script>alert(1)</script>"),
        ("image", null, "data:image/svg+xml,<svg xmlns='http://www.w3.org/2000/svg'/>"),
        ("image", XLink, "data:image/svg+xml;base64,PHN2Zy8+"),
        ("image", null, "data:image/webp;base64,UklGRg=="),
        ("image", null, "data:image/pngx;base64,AAAA"),
        ("use", null, png),
        ("a", XLink, "https://example.com/"),
        ("a", null, "javascript:alert(1)")
      )
    do assert(!SvgReading.keepsAttribute(element, namespace, "href", value), s"$element $value")
  }

  test("an Inkscape file in millimeters loads at 96 dpi without its editor's elements") {
    val bounds = SvgPicture.fromString(SvgSanitizingSuite.InkscapeFile).bounds.get
    assertEqualsDouble(bounds.upperLeftCorner.x, -396.85, 0.01)
    assertEqualsDouble(bounds.upperLeftCorner.y, -561.26, 0.01)
    assertEqualsDouble(bounds.lowerRightCorner.x, 396.85, 0.01)
    assertEqualsDouble(bounds.lowerRightCorner.y, 561.26, 0.01)
    val text = SvgDocument.normalized(SvgSanitizingSuite.InkscapeFile)
    for stripped <- Seq("namedview", "metadata", "RDF", "inkscape:label", "sodipodi:nodetypes")
    do assert(!text.contains(stripped), s"$stripped is in $text")
    assert(text.contains("fill:#ff0000"), text)
  }

  test("an Illustrator file with a DOCTYPE and entities loads, with its styles") {
    val svg = SvgPicture.fromString(SvgSanitizingSuite.IllustratorFile)
    assertEquals(svg.bounds, Some(Bounds(-10, -5, 10, 5)))
    val text = SvgDocument.normalized(SvgSanitizingSuite.IllustratorFile)
    for kept <- Seq(".st0{fill:#FF0000;}", "class=\"st0\"", "fill:#0000FF;") do
      assert(text.contains(kept), s"$kept is missing from $text")
    for stripped <- Seq("DOCTYPE", "ENTITY", "foreignObject", "pgfRef", "extraneous", "lime")
    do assert(!text.contains(stripped), s"$stripped is in $text")
  }

  test("scripts, foreign content, animations, event attributes and external links are stripped") {
    val text = SvgDocument.normalized(
      """<svg xmlns="http://www.w3.org/2000/svg" xmlns:xlink="http://www.w3.org/1999/xlink"
        xmlns:x="urn:x" width="10" height="10" onload="alert(1)" xml:base="http://example.com/">
        <script>alert(2)</script>
        <style>.a{fill:red}</style>
        <foreignObject width="10" height="10"><div xmlns="http://www.w3.org/1999/xhtml"/></foreignObject>
        <x:rect width="10" height="10"/>
        <rect id="r" class="a" width="10" height="10" x:label="label" onclick="alert(3)">
          <set attributeName="fill" to="blue"/>
          <animate attributeName="x" to="5"/>
        </rect>
        <use href="#r"/><use xlink:href="other.svg#r"/>
        <image href="http://example.com/a.png" width="1" height="1"/>
        <a href="https://example.com/"><title>a link</title></a>
      </svg>"""
    )
    for kept <- Seq(
        "<style>.a{fill:red}</style>",
        "class=\"a\"",
        "href=\"#r\"",
        "<title>a link</title>"
      )
    do assert(text.contains(kept), s"$kept is missing from $text")
    for stripped <- Seq(
        "script",
        "alert",
        "foreignObject",
        "xhtml",
        "x:rect",
        "label",
        "<set",
        "<animate",
        "other.svg",
        "example.com",
        "base"
      )
    do assert(!text.contains(stripped), s"$stripped is in $text")
  }

  test("documents with active or external content load without it") {
    for svg <- Seq(
        """<svg xmlns="http://www.w3.org/2000/svg" width="2" height="2"><script/></svg>""",
        """<svg xmlns="http://www.w3.org/2000/svg" width="2" height="2" onload="alert(1)"/>""",
        """<svg xmlns="http://www.w3.org/2000/svg" width="2" height="2"><use href="https://example.invalid/icon.svg#x"/></svg>""",
        """<svg xmlns="http://www.w3.org/2000/svg" width="2" height="2"><image href="https://example.invalid/x.png"/></svg>""",
        """<svg xmlns="http://www.w3.org/2000/svg" width="2" height="2"><path fill="url(https://example.invalid/x)"/></svg>"""
      )
    do assertEquals(SvgPicture.fromString(svg).bounds, Some(Bounds(-1, -1, 1, 1)), svg)
  }

  test("a switch keeps its first remaining child and no conditions") {
    val text = SvgDocument.normalized(
      """<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">
        <switch transform="scale(2)"><foreignObject/><?pi?><rect id="first" systemLanguage="fr"/>
        <rect id="second" requiredFeatures="x"/><g id="third"/></switch></svg>"""
    )
    assert(text.contains("""transform="scale(2)"><rect id="first"/></"""), text)
    for stripped <- Seq("second", "third", "systemLanguage", "requiredFeatures", "pi")
    do assert(!text.contains(stripped), s"$stripped is in $text")
  }

  test("an embedded SVG document is stripped") {
    val inner = java.util.Base64.getEncoder.encodeToString(
      """<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10"/>""".getBytes(
        java.nio.charset.StandardCharsets.UTF_8
      )
    )
    val text = SvgDocument.normalized(
      s"""<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">
      <image href="data:image/svg+xml;base64,$inner" width="10" height="10"/></svg>"""
    )
    assert(!text.contains("data:"), text)
  }

  test("documents that are not SVG, or nest too deep, are refused with the reason") {
    def failure(svg: String): String = intercept[IOException](SvgPicture.fromString(svg)).getMessage
    assert(
      failure("not an svg").startsWith("SVG could not be read: it is not well-formed XML ("),
      failure("not an svg")
    )
    assertEquals(
      failure("<html/>"),
      "SVG could not be read: its root element is 'html', not an SVG 'svg'"
    )
    assertEquals(
      failure("""<svg xmlns="http://www.w3.org/2000/svg" width="10" height="20000"/>"""),
      "SVG could not be read: it is 10 x 20000 pixels, and a side may be at most 16384"
    )
    assertEquals(
      failure(SvgSanitizingSuite.nested(SvgReading.MaxDepth + 1)),
      "SVG could not be read: its elements nest more than 30 deep"
    )
    val _ = intercept[IOException](SvgPicture.fromString(SvgSanitizingSuite.nested(1000)))
    assertEquals(
      SvgPicture.fromString(SvgSanitizingSuite.nested(SvgReading.MaxDepth)).bounds,
      Some(Bounds(-5, -5, 5, 5))
    )
  }
end SvgSanitizingSuite

/** SVG documents that the suites of every platform read. */
object SvgSanitizingSuite:

  /** An A4 page from Inkscape, in millimeters, with a red upper half. */
  val InkscapeFile: String =
    """<?xml version="1.0" encoding="UTF-8" standalone="no"?>
      <!-- Created with Inkscape (http://www.inkscape.org/) -->
      <svg width="210mm" height="297mm" viewBox="0 0 210 297" version="1.1" id="svg1"
        inkscape:version="1.3 (0e150ed6c4, 2023-07-21)" sodipodi:docname="drawing.svg"
        xmlns:inkscape="http://www.inkscape.org/namespaces/inkscape"
        xmlns:sodipodi="http://sodipodi.sourceforge.net/DTD/sodipodi-0.dtd"
        xmlns="http://www.w3.org/2000/svg" xmlns:svg="http://www.w3.org/2000/svg"
        xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
        xmlns:cc="http://creativecommons.org/ns#" xmlns:dc="http://purl.org/dc/elements/1.1/">
        <sodipodi:namedview id="namedview1" pagecolor="#ffffff" inkscape:document-units="mm"/>
        <defs id="defs1"/>
        <metadata id="metadata1"><rdf:RDF><cc:Work rdf:about=""><dc:format>image/svg+xml</dc:format>
        </cc:Work></rdf:RDF></metadata>
        <g inkscape:label="Layer 1" inkscape:groupmode="layer" id="layer1">
          <rect style="fill:#ff0000;stroke:none" id="rect1" width="210" height="148.5" x="0" y="0"
            sodipodi:nodetypes="cccc"/>
        </g>
      </svg>"""

  /** A 20 by 10 file from Illustrator, red on the left and blue on the right, in a `switch` after
    * the `foreignObject` that Illustrator puts first, and with a lime rect that is not drawn.
    */
  val IllustratorFile: String =
    """<?xml version="1.0" encoding="utf-8"?>
      <!-- Generator: Adobe Illustrator 16.0.0, SVG Export Plug-In . SVG Version: 6.00 Build 0) -->
      <!DOCTYPE svg PUBLIC "-//W3C//DTD SVG 1.1//EN" "http://www.w3.org/Graphics/SVG/1.1/DTD/svg11.dtd" [
        <!ENTITY ns_extend "http://ns.adobe.com/Extensibility/1.0/">
        <!ENTITY ns_ai "http://ns.adobe.com/AdobeIllustrator/10.0/">
        <!ENTITY ns_svg "http://www.w3.org/2000/svg">
        <!ENTITY ns_xlink "http://www.w3.org/1999/xlink">
        <!ENTITY fill_blue "fill:#0000FF;">
      ]>
      <svg version="1.1" xmlns:x="&ns_extend;" xmlns:i="&ns_ai;" xmlns="&ns_svg;"
        xmlns:xlink="&ns_xlink;" x="0px" y="0px" width="20px" height="10px"
        viewBox="0 0 20 10" xml:space="preserve">
        <style type="text/css">.st0{fill:#FF0000;}</style>
        <switch>
          <foreignObject requiredExtensions="&ns_ai;" x="0" y="0" width="1" height="1">
            <i:pgfRef xlink:href="#adobe_illustrator_pgf"/>
          </foreignObject>
          <g i:extraneous="self">
            <rect class="st0" width="10" height="10"/>
            <rect x="10" width="10" height="10" style="&fill_blue;"/>
          </g>
          <rect width="20" height="10" fill="lime"/>
        </switch>
      </svg>"""

  /** A red square inside `depth - 1` nested groups, `depth` deep below the root. */
  def nested(depth: Int): String =
    s"""<svg xmlns="http://www.w3.org/2000/svg" width="10" height="10">${"<g>" * (depth - 1)}
    <rect width="10" height="10" fill="red"/>${"</g>" * (depth - 1)}</svg>"""
end SvgSanitizingSuite
