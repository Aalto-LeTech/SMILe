package smile.pictures

import smile.RenderSettings
import smile.infrastructure.{CanvasPlanRenderer, SvgPlanRenderer}
import smile.testing.BrowserCapabilities

/** Every operation on the result of a long fold works on Node's stack, drawn through the canvas and
  * SVG renderers. The folds are those of [[smile.pictures.DeepFoldSuite]].
  */
class JSDeepFoldSuite extends munit.FunSuite:
  import DeepFoldSuite.*

  private val Steps = 10000

  private val SvgSteps = 300

  /** How deep the chains of bitmaps go. Every level of a chain is a bitmap with a canvas of its
    * own, and a chain of 10000 levels makes 30000 to 40000 canvases as its operations run. WebKit
    * gives out about 50000 canvases at a time and takes some ten seconds to collect the ones a test
    * has let go of, so the second chain in a row finds none left there. Drawing on a new canvas
    * then does nothing and reading it fails. On WebKit the four chains are 1000 levels deep, some
    * 14000 canvases in all.
    */
  private val ChainSteps = if BrowserCapabilities.isWebKit then 1000 else Steps

  private val settings = RenderSettings.Default

  /** `e` drawn on a canvas at 1 and 1.5 device pixels per logical pixel. */
  private def onCanvas(e: Element): Unit =
    for scale <- Seq(1.0, 1.5) do
      assert(CanvasPlanRenderer.render(e, scale, settings, 0L).buffer(settings, 0L).width > 0)

  /** `e` drawn as an `svg` element. */
  private def asSvg(e: Element): Unit =
    val root = SvgPlanRenderer.render(e, settings, timeMillis = 0L)
    assert(root.getAttribute("viewBox") != "0 0 1 1", "nothing is drawn")

  private def onCanvasAndAsSvg(e: Element): Unit =
    onCanvas(e)
    asSvg(e)

  for fold <- Folds do
    test(s"${fold.name}: every operation after $Steps steps") {
      if !fold.nestsInSvg then DeepChecks.everything(fold.flat(Steps), onCanvasAndAsSvg)
      else
        DeepChecks.everything(fold.flat(Steps), onCanvas)
        // The SVG draw runs at 300 steps, as for the nested trees below.
        DeepChecks.everything(fold.flat(SvgSteps), asSvg)
    }

  for d <- DeepTrees.nested do
    test(s"${d.name}: every operation after $Steps steps") {
      DeepChecks.everything(d.build(Steps), onCanvas)
      // The SVG draw runs at 300 levels, since jsdom nests elements in time cubic in the depth.
      DeepChecks.everything(d.build(SvgSteps), asSvg)
    }

  for d <- DeepTrees.lazyChains do
    test(s"${d.name}: every operation after $ChainSteps steps") {
      // Built once, since bitmaps of pictures made apart are not equal.
      val chain = d.build(ChainSteps)
      DeepChecks.everything(chain, onCanvasAndAsSvg)
    }
end JSDeepFoldSuite
