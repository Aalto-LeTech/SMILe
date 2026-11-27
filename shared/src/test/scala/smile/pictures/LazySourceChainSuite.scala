package smile.pictures

import smile.{RenderSettings, ResamplingMode}
import smile.infrastructure.PlatformSpecific

/** A bitmap of a picture that holds the bitmap made before it, as a program that makes a bitmap of
  * what it drew before on every tick builds, is a chain of picture sources as long as the program
  * runs. Rendering one renders first, deepest first, the renderings of the sources it draws that
  * are not kept yet, so a chain drawn fresh draws exactly what it draws when each level was drawn
  * as it was made, and renders each of its levels once.
  */
class LazySourceChainSuite extends munit.FunSuite:
  import DeepFoldSuite.green

  private val Levels = 20

  private val direct   = RenderSettings.Default
  private val stepwise = RenderSettings(resamplingMode = ResamplingMode.Stepwise)

  private val square = Rectangle(40, 40, green, None)

  /** The steps of the deep suites' chains, and steps whose levels draw the level before turned,
    * scaled, or twice.
    */
  private val chainSteps: Seq[(String, Element, (Element, Int) => Element)] =
    DeepTrees.lazySteps ++ Seq(
      ("turning", square, (a, i) => a.rotateBy(5).addToFront(DeepFoldSuite.dot(i))),
      ("scaling", square, (a, i) => a.scaleBy(1.05).addToFront(DeepFoldSuite.dot(i))),
      ("drawing twice", square, (a, _) => a.addToFront(a.moveBy(3, 2)))
    )

  /** The pixels of `e` drawn through the platform's renderer at `scale` with `settings`. */
  private def drawn(e: Element, scale: Double, settings: RenderSettings): Seq[Int] =
    PlatformSpecific.renderPlan(e, scale, settings, 0L).buffer(settings, 0L).argbData.toSeq

  for
    (name, start, step) <- chainSteps
    scale               <- Seq(1.0, 1.5)
    (mode, settings)    <- Seq("Direct" -> direct, "Stepwise" -> stepwise)
  do
    test(s"$name: a chain drawn fresh draws what it draws drawn as it grew ($scale, $mode)") {
      val grown = DeepTrees.lazyLevels(Levels, start)(step).map(drawn(_, scale, settings)).last
      val fresh = DeepTrees.lazyLevels(Levels, start)(step).last
      assertEquals(drawn(fresh, scale, settings), grown)
    }

  test("a chain drawn at one pixel per logical pixel draws what its eager renderings draw") {
    val (_, start, step) = DeepTrees.lazySteps.head
    val eager  = (0 until Levels).foldLeft(start.toBitmap())((a, i) => step(a, i).toBitmap())
    val lazily = DeepTrees.lazyLevels(Levels, start)(step).last
    assertEquals(drawn(lazily, 1, direct), drawn(eager, 1, direct))
  }

  /** The number of pixels each level below the last of the chain `levels` renders where the chain
    * is drawn fresh at one pixel per logical pixel, by level.
    */
  private def renderedPixels(levels: Vector[Bitmap]): Vector[Long] =
    val _ = drawn(levels.last, 1, direct)
    levels.init.map(_.source.renderedSourcePixelCount)

  // In these chains a level draws the level before it as it is, smaller than a tile, so it asks
  // for the whole of it at the density of the output. A mask asks through a layer and a wrap at
  // 1.5 through a tile. A turned or scaled level is asked for at another density, and a clipped
  // one for a part.
  private val wholeAsked = Set("adding in front", "masking", "wrapping", "drawing twice")

  for
    (name, start, step) <- chainSteps if wholeAsked(name)
    scale               <- Seq(1.0, 1.5)
  do
    test(s"$name: a chain drawn fresh renders each level once ($scale)") {
      val levels = DeepTrees.lazyLevels(Levels, start)(step)
      val _      = drawn(levels.last, scale, direct)
      // A wrap's tile at 1.5 stretches the period's 16.5 device pixels to 17, which asks for the
      // level inside it at 1.75 buffer pixels per pixel.
      val density =
        if name == "wrapping" && scale == 1.5 then 1.75 else PixelSource.contentDensity(scale)
      // Counts on RasterCache.shared keeping what was rendered between the draws, which fits
      // its budget many times over.
      for (level, i) <- levels.init.zipWithIndex do
        val source          = level.source
        val rendered        = source.renderedSourcePixelCount
        val (width, height) = source.pixelSize
        val kept            = source.contentPixelsCovering(0, 0, width, height, density, direct, 0L)
        val pixels          = kept.map((_, buffer) => buffer.width.toLong * buffer.height)
        assertEquals(source.renderedSourcePixelCount, rendered, s"level $i is rendered again")
        assertEquals(Some(rendered), pixels, s"level $i")
    }

  test("a chain whose levels clip the level before renders only the tiles that show, once") {
    // Each level shows columns 300 to 700 of the 1000 by 4 pixels of the level before, which lie
    // in the tiles from column 256 to 768.
    val (_, start, step) = DeepTrees.lazySteps(1)
    val levels           = DeepTrees.lazyLevels(Levels, start)(step)
    assertEquals(renderedPixels(levels), Vector.fill(Levels)(512L * 4))
  }
end LazySourceChainSuite
