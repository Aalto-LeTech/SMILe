package smile.colors

/** Represents a predefined color with a canonical name, extending the `Color` class.
  *
  * @constructor
  *   Creates a `PresetColor` from an ARGB integer and a canonical name.
  * @param argbInt
  *   The ARGB integer representing the color.
  * @param canonicalName
  *   The canonical name for the color.
  */
class PresetColor(argbInt: Int, canonicalName: String) extends Color(argbInt, Some(canonicalName)):

  /** Auxiliary constructor for creating a `PresetColor` from an existing `Color` instance and a
    * canonical name. This constructor facilitates the easy labeling of colors with descriptive
    * names.
    *
    * @param color
    *   The `Color` instance to be associated with a canonical name.
    * @param canonicalName
    *   The canonical name for the color.
    */
  def this(color: Color, canonicalName: String) = this(color.toARGBInt, canonicalName)

/** Companion object for the `PresetColor` class, defining a collection of predefined colors with
  * canonical names.
  */
object PresetColor:
  val AliceBlue: PresetColor = PresetColor(0xfff0f8ff, "alice blue")

  val AlizarinCrimson: PresetColor = PresetColor(0xff4e1500, "alizarin crimson")

  val Amethyst: PresetColor = PresetColor(0xff9966cc, "amethyst")

  val AntiqueWhite: PresetColor = PresetColor(0xfffaebd7, "antique white")

  val Aqua: PresetColor = PresetColor(0xff00ffff, "aqua")

  val Aquamarine: PresetColor = PresetColor(0xff7fffd4, "aquamarine")

  val Azure: PresetColor = PresetColor(0xfff0ffff, "azure")

  val Beige: PresetColor = PresetColor(0xfff5f5dc, "beige")

  val Bisque: PresetColor = PresetColor(0xffffe4c4, "bisque")

  val Black: PresetColor = PresetColor(0xff000000, "black")

  val BlanchedAlmond: PresetColor = PresetColor(0xffffebcd, "blanched almond")

  val Blue: PresetColor = PresetColor(0xff0000ff, "blue")

  val BlueViolet: PresetColor = PresetColor(0xff8a2be2, "blue violet")

  val Brown: PresetColor = PresetColor(0xffa52a2a, "brown")

  val BurlyWood: PresetColor = PresetColor(0xffdeb887, "burly wood")

  val CadetBlue: PresetColor = PresetColor(0xff5f9ea0, "cadet blue")

  val Chartreuse: PresetColor = PresetColor(0xff7fff00, "chartreuse")

  val Chocolate: PresetColor = PresetColor(0xffd2691e, "chocolate")

  val Coral: PresetColor = PresetColor(0xffff7f50, "coral")

  val CornflowerBlue: PresetColor = PresetColor(0xff6495ed, "cornflower blue")

  val CornSilk: PresetColor = PresetColor(0xfffff8dc, "cornsilk")

  val Crimson: PresetColor = PresetColor(0xffdc143c, "crimson")

  val Cyan: PresetColor = PresetColor(0xff00ffff, "cyan")

  val DarkBlue: PresetColor = PresetColor(0xff00008b, "dark blue")

  val DarkCyan: PresetColor = PresetColor(0xff008b8b, "dark cyan")

  val DarkGoldenrod: PresetColor = PresetColor(0xffb8860b, "dark goldenrod")

  val DarkGray: PresetColor = PresetColor(0xffa9a9a9, "dark gray")

  val DarkGreen: PresetColor = PresetColor(0xff006400, "dark green")

  val DarkGrey: Color = PresetColor(0xffa9a9a9, "dark grey")

  val DarkKhaki: PresetColor = PresetColor(0xffbdb76b, "dark khaki")

  val DarkMagenta: PresetColor = PresetColor(0xff8b008b, "dark magenta")

  val DarkOliveGreen: PresetColor = PresetColor(0xff556b2f, "dark olive green")

  val DarkOrange: PresetColor = PresetColor(0xffff8c00, "dark orange")

  val DarkOrchid: PresetColor = PresetColor(0xff9932cc, "dark orchid")

  val DarkRed: PresetColor = PresetColor(0xff8b0000, "dark red")

  val DarkSalmon: PresetColor = PresetColor(0xffe9967a, "dark salmon")

  val DarkSeaGreen: PresetColor = PresetColor(0xff8fbc8f, "dark sea green")

  val DarkSlateBlue: PresetColor = PresetColor(0xff483d8b, "dark slate blue")

  val DarkSlateGray: PresetColor = PresetColor(0xff2f4f4f, "dark slate gray")

  val DarkSlateGrey: PresetColor = PresetColor(0xff2f4f4f, "dark slate grey")

  val DarkTurquoise: PresetColor = PresetColor(0xff00ced1, "dark turquoise")

  val DarkViolet: PresetColor = PresetColor(0xff9400d3, "dark violet")

  val DeepPink: PresetColor = PresetColor(0xffff1493, "deep pink")

  val DeepSkyBlue: PresetColor = PresetColor(0xff00bfff, "deep sky blue")

  val DimGray: PresetColor = PresetColor(0xff696969, "dim gray")

  val DimGrey: PresetColor = PresetColor(0xff696969, "dim grey")

  val DodgerBlue: PresetColor = PresetColor(0xff1e90ff, "dodger blue")

  val FireBrick: PresetColor = PresetColor(0xffb22222, "fire brick")

  val FloralWhite: PresetColor = PresetColor(0xfffffaf0, "floral white")

  val ForestGreen: PresetColor = PresetColor(0xff228b22, "forest green")

  val Fuchsia: PresetColor = PresetColor(0xffff00ff, "fuchsia")

  val Gainsboro: PresetColor = PresetColor(0xffdcdcdc, "gainsboro")

  val GhostWhite: PresetColor = PresetColor(0xfff8f8ff, "ghost white")

  val Gold: PresetColor = PresetColor(0xffffd700, "gold")

  val Goldenrod: PresetColor = PresetColor(0xffdaa520, "goldenrod")

  val Gray: PresetColor = PresetColor(0xff808080, "gray")

  val Green: PresetColor = PresetColor(0xff008000, "green")

  val GreenYellow: PresetColor = PresetColor(0xffadff2f, "green yellow")

  val Grey: PresetColor = PresetColor(0xff808080, "grey")

  val Honeydew: PresetColor = PresetColor(0xfff0fff0, "honeydew")

  val HotPink: PresetColor = PresetColor(0xffff69b4, "hot pink")

  val IndianRed: PresetColor = PresetColor(0xffcd5c5c, "indian red")

  val Indigo: PresetColor = PresetColor(0xff4b0082, "indigo")

  val Ivory: PresetColor = PresetColor(0xfffffff0, "ivory")

  val Khaki: PresetColor = PresetColor(0xfff0e68c, "khaki")

  val Lavender: PresetColor = PresetColor(0xffe6e6fa, "lavender")

  val LavenderBlush: PresetColor = PresetColor(0xfffff0f5, "lavender blush")

  val LawnGreen: PresetColor = PresetColor(0xff7cfc00, "lawn green")

  val LemonChiffon: PresetColor = PresetColor(0xfffffacd, "lemon chiffon")

  val LightBlue: PresetColor = PresetColor(0xffadd8e6, "light blue")

  val LightCoral: PresetColor = PresetColor(0xfff08080, "light coral")

  val LightCyan: PresetColor = PresetColor(0xffe0ffff, "light cyan")

  val LightGoldenrodYellow: PresetColor =
    PresetColor(0xfffafad2, "light goldenrod yellow")

  val LightGray: PresetColor = PresetColor(0xffd3d3d3, "light gray")

  val LightGreen: PresetColor = PresetColor(0xff90ee90, "light green")

  val LightGrey: PresetColor = PresetColor(0xffd3d3d3, "light grey")

  val LightPink: PresetColor = PresetColor(0xffffb6c1, "light pink")

  val LightSalmon: PresetColor = PresetColor(0xffffa07a, "light salmon")

  val LightSeaGreen: PresetColor = PresetColor(0xff20b2aa, "light sea green")

  val LightSkyBlue: PresetColor = PresetColor(0xff87cefa, "light sky blue")

  val LightSlateGray: PresetColor = PresetColor(0xff778899, "light slate gray")

  val LightSlateGrey: PresetColor = PresetColor(0xff778899, "light slate grey")

  val LightSteelBlue: PresetColor = PresetColor(0xffb0c4de, "light steel blue")

  val LightYellow: PresetColor = PresetColor(0xffffffe0, "light yellow")

  val Lime: PresetColor = PresetColor(0xff00ff00, "lime")

  val LimeGreen: PresetColor = PresetColor(0xff32cd32, "lime green")

  val Linen: PresetColor = PresetColor(0xfffaf0e6, "linen")

  val Magenta: PresetColor = PresetColor(0xffff00ff, "magenta")

  val Maroon: PresetColor = PresetColor(0xff800000, "maroon")

  val MediumAquamarine: PresetColor = PresetColor(0xff66cdaa, "medium aquamarine")

  val MediumBlue: PresetColor = PresetColor(0xff0000cd, "medium blue")

  val MediumOrchid: PresetColor = PresetColor(0xffba55d3, "medium orchid")

  val MediumPurple: PresetColor = PresetColor(0xff9370db, "medium purple")

  val MediumSeaGreen: PresetColor = PresetColor(0xff3cb371, "medium sea green")

  val MediumSlateBlue: PresetColor = PresetColor(0xff7b68ee, "medium slate blue")

  val MediumSpringGreen: PresetColor = PresetColor(0xff00fa9a, "medium spring green")

  val MediumTurquoise: PresetColor = PresetColor(0xff48d1cc, "medium turquoise")

  val MediumVioletRed: PresetColor = PresetColor(0xffc71585, "medium violet red")

  val MidnightBlue: PresetColor = PresetColor(0xff191970, "midnight blue")

  val MintCream: PresetColor = PresetColor(0xfff5fffa, "mint cream")

  val MistyRose: PresetColor = PresetColor(0xffffe4e1, "misty rose")

  val Moccasin: PresetColor = PresetColor(0xffffe4b5, "moccasin")

  val NavajoWhite: PresetColor = PresetColor(0xffffdead, "navajo white")

  val Navy: PresetColor = PresetColor(0xff000080, "navy")

  val OldLace: PresetColor = PresetColor(0xfffdf5e6, "old lace")

  val Olive: PresetColor = PresetColor(0xff808000, "olive")

  val OliveDrab: PresetColor = PresetColor(0xff6b8e23, "olive drab")

  val Orange: PresetColor = PresetColor(0xffffa500, "orange")

  val OrangeRed: PresetColor = PresetColor(0xffff4500, "orange red")

  val Orchid: PresetColor = PresetColor(0xffda70d6, "orchid")

  val PaleGoldenrod: PresetColor = PresetColor(0xffeee8aa, "pale goldenrod")

  val PaleGreen: PresetColor = PresetColor(0xff98fb98, "pale green")

  val PaleTurquoise: PresetColor = PresetColor(0xffafeeee, "pale turquoise")

  val PaleVioletRed: PresetColor = PresetColor(0xffdb7093, "pale violet red")

  val PapayaWhip: PresetColor = PresetColor(0xffffefd5, "papaya whip")

  val PeachPuff: PresetColor = PresetColor(0xffffdab9, "peach puff")

  val Peru: PresetColor = PresetColor(0xffcd853f, "peru")

  val Pink: PresetColor = PresetColor(0xffffc0cb, "pink")

  val Plum: PresetColor = PresetColor(0xffdda0dd, "plum")

  val PowderBlue: PresetColor = PresetColor(0xffb0e0e6, "powder blue")

  val Purple: PresetColor = PresetColor(0xff800080, "purple")

  val Red: PresetColor = PresetColor(0xffff0000, "red")

  val RosyBrown: PresetColor = PresetColor(0xffbc8f8f, "rosy brown")

  val RoyalBlue: PresetColor = PresetColor(0xff4169e1, "royal blue")

  val SaddleBrown: PresetColor = PresetColor(0xff8b4513, "saddle brown")

  val Salmon: PresetColor = PresetColor(0xfffa8072, "salmon")

  val SandyBrown: PresetColor = PresetColor(0xfff4a460, "sandy brown")

  val SeaGreen: PresetColor = PresetColor(0xff2e8b57, "sea green")

  val SeaShell: PresetColor = PresetColor(0xfffff5ee, "seashell")

  val Sienna: PresetColor = PresetColor(0xffa0522d, "sienna")

  val Silver: PresetColor = PresetColor(0xffc0c0c0, "silver")

  val SkyBlue: PresetColor = PresetColor(0xff87ceeb, "sky blue")

  val SlateBlue: PresetColor = PresetColor(0xff6a5acd, "slate blue")

  val SlateGray: PresetColor = PresetColor(0xff708090, "slate gray")

  val SlateGrey: PresetColor = PresetColor(0xff708090, "slate grey")

  val Snow: PresetColor = PresetColor(0xfffffafa, "snow")

  val SpringGreen: PresetColor = PresetColor(0xff00ff7f, "spring green")

  val SteelBlue: PresetColor = PresetColor(0xff4682b4, "steel blue")

  val Tan: PresetColor = PresetColor(0xffd2b48c, "tan")

  val Teal: PresetColor = PresetColor(0xff008080, "teal")

  val Thistle: PresetColor = PresetColor(0xffd8bfd8, "thistle")

  val Tomato: PresetColor = PresetColor(0xffff6347, "tomato")

  val Turquoise: PresetColor = PresetColor(0xff40e0d0, "turquoise")

  val Violet: PresetColor = PresetColor(0xffee82ee, "violet")

  val Wheat: PresetColor = PresetColor(0xfff5deb3, "wheat")

  val White: PresetColor = PresetColor(0xffffffff, "white")

  val WhiteSmoke: PresetColor = PresetColor(0xfff5f5f5, "white smoke")

  val Yellow: PresetColor = PresetColor(0xffffff00, "yellow")

  val YellowGreen: PresetColor = PresetColor(0xff9acd32, "yellow green")

  val AfricanGreen: PresetColor = PresetColor(Color.argbIntFrom(49, 148, 0, 255), "African green")

  val AfricanRed: PresetColor = PresetColor(Color.argbIntFrom(149, 32, 56, 255), "African red")

  val AfricanYellow: PresetColor =
    PresetColor(Color.argbIntFrom(247, 198, 8, 255), "African yellow")

  val AmericanBlue: PresetColor = PresetColor(Color.argbIntFrom(60, 59, 110, 255), "American blue")

  val AmericanRed: PresetColor = PresetColor(Color.argbIntFrom(178, 34, 52, 255), "American red")

  val AustralianBlue: PresetColor =
    PresetColor(Color.argbIntFrom(0, 0, 139, 255), "Australian blue")

  val AustralianRed: PresetColor = PresetColor(Color.argbIntFrom(255, 0, 0, 255), "Australian red")

  val AustrianRed: PresetColor = PresetColor(Color.argbIntFrom(237, 41, 57, 255), "Austrian red")

  val BelgianRed: PresetColor = PresetColor(Color.argbIntFrom(237, 41, 57, 255), "Belgian red")

  val BelgianYellow: PresetColor =
    PresetColor(Color.argbIntFrom(250, 224, 66, 255), "Belgian yellow")

  val BrazilianBlue: PresetColor = PresetColor(Color.argbIntFrom(0, 39, 118, 255), "Brazilian blue")

  val BrazilianGreen: PresetColor =
    PresetColor(Color.argbIntFrom(0, 155, 58, 255), "Brazilian green")

  val BritishBlue: PresetColor = PresetColor(Color.argbIntFrom(0, 36, 125, 255), "British blue")

  val BritishRed: PresetColor = PresetColor(Color.argbIntFrom(207, 20, 43, 255), "British red")

  val BrazilianYellow: PresetColor =
    PresetColor(Color.argbIntFrom(254, 223, 0, 255), "Brazilian yellow")

  val CanadianRed: PresetColor = PresetColor(Color.argbIntFrom(255, 0, 0, 255), "Canadian red")

  val ChineseBlue: PresetColor = PresetColor(Color.argbIntFrom(0, 0, 149, 255), "Chinese blue")

  val ChineseRed: PresetColor = PresetColor(Color.argbIntFrom(254, 0, 0, 255), "Chinese red")

  val CzechBlue: PresetColor = PresetColor(Color.argbIntFrom(17, 69, 126, 255), "Czech blue")

  val CzechRed: PresetColor = PresetColor(Color.argbIntFrom(215, 20, 26, 255), "Czech red")

  val EuropeanBlue: PresetColor = PresetColor(Color.argbIntFrom(0, 51, 153, 255), "European blue")

  val EuropeanYellow: PresetColor =
    PresetColor(Color.argbIntFrom(255, 204, 0, 255), "European yellow")

  val DanishRed: PresetColor = PresetColor(Color.argbIntFrom(198, 12, 48, 255), "Danish red")

  val FinnishBlue: PresetColor = PresetColor(Color.argbIntFrom(0, 63, 135, 255), "Finnish blue")

  val FrenchBlue: PresetColor = PresetColor(Color.argbIntFrom(0, 35, 149, 255), "French blue")

  val FrenchRed: PresetColor = PresetColor(Color.argbIntFrom(237, 41, 57, 255), "French red")

  val GermanRed: PresetColor = PresetColor(Color.argbIntFrom(221, 0, 0, 255), "German red")

  val GermanYellow: PresetColor = PresetColor(Color.argbIntFrom(255, 206, 0, 255), "German yellow")

  val GreekBlue: PresetColor = PresetColor(Color.argbIntFrom(13, 94, 175, 255), "Greek blue")

  val GreenlandicRed: PresetColor =
    PresetColor(Color.argbIntFrom(192, 12, 48, 255), "Greenlandic red")

  val HungarianGreen: PresetColor =
    PresetColor(Color.argbIntFrom(67, 111, 77, 255), "Hungarian green")

  val HungarianRed: PresetColor = PresetColor(Color.argbIntFrom(205, 42, 62, 255), "Hungarian red")

  val IcelandicBlue: PresetColor = PresetColor(Color.argbIntFrom(0, 56, 151, 255), "Icelandic blue")

  val IcelandicRed: PresetColor = PresetColor(Color.argbIntFrom(215, 40, 40, 255), "Icelandic red")

  val IrishGreen: PresetColor = PresetColor(Color.argbIntFrom(22, 155, 98, 255), "Irish green")

  val IrishOrange: PresetColor = PresetColor(Color.argbIntFrom(255, 136, 62, 255), "Irish orange")

  val ItalianGreen: PresetColor = PresetColor(Color.argbIntFrom(0, 146, 70, 255), "Italian green")

  val ItalianRed: PresetColor = PresetColor(Color.argbIntFrom(206, 43, 55, 255), "Italian red")

  val JapaneseRed: PresetColor = PresetColor(Color.argbIntFrom(188, 0, 45, 255), "Japanese red")

  val NorwegianBlue: PresetColor = PresetColor(Color.argbIntFrom(0, 40, 104, 255), "Norwegian blue")

  val NorwegianRed: PresetColor = PresetColor(Color.argbIntFrom(239, 43, 45, 255), "Norwegian red")

  val RomanianBlue: PresetColor = PresetColor(Color.argbIntFrom(0, 43, 127, 255), "Romanian blue")

  val RomanianRed: PresetColor = PresetColor(Color.argbIntFrom(206, 17, 38, 255), "Romanian red")

  val RomanianYellow: PresetColor =
    PresetColor(Color.argbIntFrom(252, 209, 22, 255), "Romanian yellow")

  val RussianBlue: PresetColor = PresetColor(Color.argbIntFrom(0, 57, 166, 255), "Russian blue")

  val RussianRed: PresetColor = PresetColor(Color.argbIntFrom(213, 43, 30, 255), "Russian red")

  val SomaliBlue: PresetColor = PresetColor(Color.argbIntFrom(65, 137, 221, 255), "Somali blue")

  val SwedishBlue: PresetColor = PresetColor(Color.argbIntFrom(0, 107, 168, 255), "Swedish blue")

  val SwedishYellow: PresetColor =
    PresetColor(Color.argbIntFrom(254, 205, 1, 255), "Swedish yellow")

  val VietnameseRed: PresetColor =
    PresetColor(Color.argbIntFrom(218, 37, 29, 255), "Vietnamese red")

  val VietnameseYellow: PresetColor =
    PresetColor(Color.argbIntFrom(255, 255, 0, 255), "Vietnamese yellow")

  /** Color constant for 10-percent black. */
  val Black10: PresetColor = PresetColor(Black.tintByPercentage(90), "black10")

  /** Color constant for 20-percent black. */
  val Black20: PresetColor = PresetColor(Black.tintByPercentage(80), "black20")

  /** Color constant for 30-percent black. */
  val Black30: PresetColor = PresetColor(Black.tintByPercentage(70), "black30")

  /** Color constant for 40-percent black. */
  val Black40: PresetColor = PresetColor(Black.tintByPercentage(60), "black40")

  /** Color constant for 50-percent black. */
  val Black50: PresetColor = PresetColor(Black.tintByPercentage(50), "black50")

  /** Color constant for 60-percent black. */
  val Black60: PresetColor = PresetColor(Black.tintByPercentage(40), "black60")

  /** Color constant for 70-percent black. */
  val Black70: PresetColor = PresetColor(Black.tintByPercentage(30), "black70")

  /** Color constant for 80-percent black. */
  val Black80: PresetColor = PresetColor(Black.tintByPercentage(20), "black80")

  /** Color constant for 90-percent black. */
  val Black90: PresetColor = PresetColor(Black.tintByPercentage(10), "black90")

  /** Color constant for 10-percent white. */
  val White10: PresetColor = PresetColor(White.shadeByPercentage(90), "white10")

  /** Color constant for 20-percent white. */
  val White20: PresetColor = PresetColor(White.shadeByPercentage(80), "white20")

  /** Color constant for 30-percent white. */
  val White30: PresetColor = PresetColor(White.shadeByPercentage(70), "white30")

  /** Color constant for 40-percent white. */
  val White40: PresetColor = PresetColor(White.shadeByPercentage(60), "white40")

  /** Color constant for 50-percent white. */
  val White50: PresetColor = PresetColor(White.shadeByPercentage(50), "white50")

  /** Color constant for 60-percent white. */
  val White60: PresetColor = PresetColor(White.shadeByPercentage(40), "white60")

  /** Color constant for 70-percent white. */
  val White70: PresetColor = PresetColor(White.shadeByPercentage(30), "white70")

  /** Color constant for 80-percent white. */
  val White80: PresetColor = PresetColor(White.shadeByPercentage(20), "white80")

  /** Color constant for 90-percent white. */
  val White90: PresetColor = PresetColor(White.shadeByPercentage(10), "white90")

  /** Color constant for transparent <em>"white"</em>. */
  val Transparent: PresetColor = PresetColor(Color.argbIntFrom(255, 255, 255, 0), "transparent")
