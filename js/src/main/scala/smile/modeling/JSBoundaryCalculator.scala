package smile.modeling

import org.scalajs.dom
import org.scalajs.dom.document
import scalatags.JsDom.implicits.{doubleAttr, intAttr, stringAttr, stringFrag}
import scalatags.JsDom.svgAttrs.{fontFamily, fontSize, fontWeight, xmlns}
import scalatags.JsDom.svgTags.{svg, text}
import smile.pictures.Text

object JSBoundaryCalculator:
  /** Calculates the bounding box for a given text object.
    *
    * @param element
    *   The `Text` object for which to calculate the bounding box.
    * @return
    *   The `Bounds` representing the bounding box of the text.
    */
  def fromText(element: Text): Bounds =
    val mySvg = svg(
      xmlns := "http://www.w3.org/2000/svg",
      text(
        fontFamily := s""""${element.typeface}"""",
        fontWeight := element.weight,
        fontSize   := element.size,
        element.content
      )
    ).render
    mySvg.setAttribute("visibility", "hidden")
    document.body.appendChild(mySvg)
    val boundingRect = mySvg.asInstanceOf[dom.SVGLocatable].getBBox()
    val width        = boundingRect.width
    val height       = boundingRect.height
    document.body.removeChild(mySvg)
    val x = element.position.x
    val y = element.position.y
    Bounds(x, y, x + width, y + height)
