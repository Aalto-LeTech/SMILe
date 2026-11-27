package smile.testing

import smile.infrastructure.NativeBuffer

/** The middle of `buffer`'s pixels, each weighing what `weight` gives for its packed ARGB value, in
  * pixels from the buffer's upper left corner.
  */
def weightedMiddle(buffer: NativeBuffer, weight: Int => Double): (Double, Double) =
  val argb                    = buffer.argbData
  var (sumX, sumY, sumWeight) = (0.0, 0.0, 0.0)
  for y <- 0 until buffer.height; x <- 0 until buffer.width do
    val w = weight(argb(y * buffer.width + x))
    sumX += w * (x + 0.5)
    sumY += w * (y + 0.5)
    sumWeight += w
  (sumX / sumWeight, sumY / sumWeight)
