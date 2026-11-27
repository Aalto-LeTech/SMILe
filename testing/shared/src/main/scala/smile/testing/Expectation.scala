package smile.testing

/** How closely two backends are expected to agree on a case. [[smile.testing.ParityMetric]] says
  * what each profile checks and with which numbers.
  */
enum Expectation derives CanEqual:

  /** The same rasterizer drew both pictures, so no pixel differs by more than a rounding step. */
  case Exact

  /** Two rasterizers drew flat colors. Everything agrees but the antialiased edges, and those agree
    * on where they are.
    */
  case AA

  /** As [[smile.testing.Expectation.AA]], but the colors are interpolated, so areas away from the
    * edges may differ by the rounding of two gradient implementations.
    */
  case Gradient

  /** Text across two platforms. Each lays out and measures glyphs with a font engine of its own, so
    * only the ink's place and amount are compared, within `slack` pixels.
    */
  case Layout(slack: Double)

  /** The backends are known to disagree here, for the stated reason, but no further than `bound`
    * with `limits` allows. The pair is compared against `otherwise`, the profile that would hold
    * without the disagreement, and, where it does not meet that, against the bound. It passes
    * either way, and the report of [[smile.testing.ParityMetric.judge]] says which. A difference
    * past the bound fails. Platforms and browser versions that disagree less, or not at all, pass
    * too.
    */
  case KnownDiffers(
      reason: String,
      otherwise: Expectation,
      bound: Expectation,
      limits: ParityMetric.Limits = ParityMetric.Limits()
  )
