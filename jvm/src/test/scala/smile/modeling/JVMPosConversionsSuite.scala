package smile.modeling

import smile.modeling.NumberSupport.relaidAsOnTheJVM

import java.awt.Point
import scala.util.Random

/** The JVM-only `Pos(point)`, and the JVM's own `Double.toString` as the reference the shared
  * number layout copies.
  */
class JVMPosConversionsSuite extends munit.FunSuite:

  test("Pos(point) takes an AWT point's coordinates") {
    assertEquals(Pos(new Point(3, -4)), Pos(3, -4))
    val point: Point = new Point(7, 8)
    assertEquals(Pos(point).toString, "(7.0,8.0)")
  }

  test("relaying a number's text as on the JVM leaves the JVM's own text as it is") {
    val random  = new Random(20261118)
    val special = Seq(Double.MinPositiveValue, Double.MaxValue, 1e-3, 1e7, 9.999999999999999e6)
    val values  = special ++ Seq.fill(5000) {
      java.lang.Double.longBitsToDouble(random.nextLong())
    } ++ Seq.fill(5000)((random.nextDouble() - 0.5) * math.pow(10, random.nextInt(24) - 12))
    for value <- values do assertEquals(relaidAsOnTheJVM(value), value.toString)
  }
