package aalto.smcl.common


import aalto.smcl.SMCL




/**
 *
 *
 * @author Aleksi Lukkarinen
 */
object RGBAComponentTranslationTable {

  SMCL.performInitialization()


  /**
   *
   *
   * @param reds
   * @param greens
   * @param blues
   * @param opacities
   * @return
   */
  def apply(
    reds: Seq[Short],
    greens: Seq[Short],
    blues: Seq[Short],
    opacities: Seq[Short]): RGBAComponentTranslationTable = {

    RGBATranslationTableValidator.validateSeparateDimensions(reds, greens, blues, opacities)

    new RGBAComponentTranslationTable(Seq(reds, greens, blues, opacities))
  }

  /**
   *
   *
   * @param valueProvider
   * @return
   */
  def apply(valueProvider: Short => (Short, Short, Short, Short)): RGBAComponentTranslationTable = {
    require(valueProvider != null, "Value provider function argument cannot be null.")

    val tableArray = Array.ofDim[Short](4, ByteRange.length)

    for (rowIndex <- ByteRange) {
      val rgbaRowCandidate: (Short, Short, Short, Short) = valueProvider(rowIndex.toShort)

      RGBATranslationTableValidator.validateFunctionProvidedComponents(rgbaRowCandidate)

      tableArray(0)(rowIndex) = rgbaRowCandidate._1
      tableArray(1)(rowIndex) = rgbaRowCandidate._2
      tableArray(2)(rowIndex) = rgbaRowCandidate._3
      tableArray(3)(rowIndex) = rgbaRowCandidate._4
    }

    val tableSeq: Seq[Seq[Short]] = tableArray.map(_.toList).toList

    new RGBAComponentTranslationTable(tableSeq)
  }

  /** */
  lazy val forNegation: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      ((ColorValidator.MaximumRgbRed - index).toShort,
        (ColorValidator.MaximumRgbGreen - index).toShort,
        (ColorValidator.MaximumRgbBlue - index).toShort,
        index)
    }
  }

  /** */
  lazy val forNegatingRed: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      ((ColorValidator.MaximumRgbRed - index).toShort,
        index,
        index,
        index)
    }
  }

  /** */
  lazy val forNegatingRedAndGreen: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      ((ColorValidator.MaximumRgbRed - index).toShort,
        (ColorValidator.MaximumRgbGreen - index).toShort,
        index,
        index)
    }
  }

  /** */
  lazy val forNegatingGreen: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      (index,
        (ColorValidator.MaximumRgbGreen - index).toShort,
        index,
        index)
    }
  }

  /** */
  lazy val forNegatingGreenAndBlue: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      (index,
        (ColorValidator.MaximumRgbGreen - index).toShort,
        (ColorValidator.MaximumRgbBlue - index).toShort,
        index)
    }
  }

  /** */
  lazy val forNegatingBlue: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      (index,
        index,
        (ColorValidator.MaximumRgbBlue - index).toShort,
        index)
    }
  }

  /** */
  lazy val forNegatingRedAndBlue: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      ((ColorValidator.MaximumRgbRed - index).toShort,
        index,
        (ColorValidator.MaximumRgbBlue - index).toShort,
        index)
    }
  }

  /** */
  lazy val forKeepingOnlyRed: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      (index, 0, 0, index)
    }
  }

  /** */
  lazy val forKeepingOnlyRedAndGreen: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      (index, index, 0, index)
    }
  }

  /** */
  lazy val forKeepingOnlyGreen: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      (0, index, 0, index)
    }
  }

  /** */
  lazy val forKeepingOnlyGreenAndBlue: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      (0, index, index, index)
    }
  }

  /** */
  lazy val forKeepingOnlyBlue: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      (0, 0, index, index)
    }
  }

  /** */
  lazy val forKeepingOnlyRedAndBlue: RGBAComponentTranslationTable = {
    RGBAComponentTranslationTable {index =>
      (index, 0, index, index)
    }
  }

  /**
   *
   *
   * @param strengthAsPercentage
   * @return
   */
  def forPosterization(strengthAsPercentage: Int): RGBAComponentTranslationTable = {
    CommonValidators.validatePercentage(strengthAsPercentage, Option("Strength"))

    val divisor = strengthAsPercentage + 1

    RGBAComponentTranslationTable {index =>
      val v = (index - index % divisor).toShort
      (v, v, v, index)
    }
  }

}


/**
 *
 *
 * @author Aleksi Lukkarinen
 */
case class RGBAComponentTranslationTable private(table: Seq[Seq[Short]])
  extends RGBAColorTranslator with Immutable {

  /**
   *
   *
   * @param red
   * @param green
   * @param blue
   * @param opacity
   * @return
   */
  def translate(red: Int, green: Int, blue: Int, opacity: Int): (Int, Int, Int, Int) = {
    ColorValidator.validateRgbaColor(red, green, blue, opacity)

    (table.head(red.toShort),
      table(1)(green.toShort),
      table(2)(blue.toShort),
      table.last(opacity.toShort))
  }

  /**
   *
   *
   * @return
   */
  def toArray: Array[Array[Short]] = table.map(_.toArray).toArray

}
