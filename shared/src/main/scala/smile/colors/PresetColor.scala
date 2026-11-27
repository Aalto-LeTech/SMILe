package smile.colors

import scala.collection.immutable.ListMap

/** Predefined colors, plain [[smile.colors.Color]] values, and their canonical names. Each is shown
  * with its color and its red, green and blue as hexadecimal digits.
  *
  * @see
  *   [[smile.colors.PresetColor.byName]]
  */
object PresetColor:
  private val registry = ListMap.newBuilder[String, Color]

  private def preset(argb: Int, name: String): Color = preset(new Color(argb), name)

  private def preset(color: Color, name: String): Color =
    registry += name -> color
    color

  /** <span style="background:#f0f8ff;border:1px solid gray">&emsp;&emsp;</span> `#f0f8ff` */
  val AliceBlue: Color = preset(0xfff0f8ff, "alice blue")

  /** <span style="background:#4e1500;border:1px solid gray">&emsp;&emsp;</span> `#4e1500` */
  val AlizarinCrimson: Color = preset(0xff4e1500, "alizarin crimson")

  /** <span style="background:#9966cc;border:1px solid gray">&emsp;&emsp;</span> `#9966cc` */
  val Amethyst: Color = preset(0xff9966cc, "amethyst")

  /** <span style="background:#faebd7;border:1px solid gray">&emsp;&emsp;</span> `#faebd7` */
  val AntiqueWhite: Color = preset(0xfffaebd7, "antique white")

  /** <span style="background:#00ffff;border:1px solid gray">&emsp;&emsp;</span> `#00ffff` */
  val Aqua: Color = preset(0xff00ffff, "aqua")

  /** <span style="background:#7fffd4;border:1px solid gray">&emsp;&emsp;</span> `#7fffd4` */
  val Aquamarine: Color = preset(0xff7fffd4, "aquamarine")

  /** <span style="background:#f0ffff;border:1px solid gray">&emsp;&emsp;</span> `#f0ffff` */
  val Azure: Color = preset(0xfff0ffff, "azure")

  /** <span style="background:#f5f5dc;border:1px solid gray">&emsp;&emsp;</span> `#f5f5dc` */
  val Beige: Color = preset(0xfff5f5dc, "beige")

  /** <span style="background:#ffe4c4;border:1px solid gray">&emsp;&emsp;</span> `#ffe4c4` */
  val Bisque: Color = preset(0xffffe4c4, "bisque")

  /** <span style="background:#000000;border:1px solid gray">&emsp;&emsp;</span> `#000000` */
  val Black: Color = preset(0xff000000, "black")

  /** <span style="background:#ffebcd;border:1px solid gray">&emsp;&emsp;</span> `#ffebcd` */
  val BlanchedAlmond: Color = preset(0xffffebcd, "blanched almond")

  /** <span style="background:#0000ff;border:1px solid gray">&emsp;&emsp;</span> `#0000ff` */
  val Blue: Color = preset(0xff0000ff, "blue")

  /** <span style="background:#8a2be2;border:1px solid gray">&emsp;&emsp;</span> `#8a2be2` */
  val BlueViolet: Color = preset(0xff8a2be2, "blue violet")

  /** <span style="background:#a52a2a;border:1px solid gray">&emsp;&emsp;</span> `#a52a2a` */
  val Brown: Color = preset(0xffa52a2a, "brown")

  /** <span style="background:#deb887;border:1px solid gray">&emsp;&emsp;</span> `#deb887` */
  val BurlyWood: Color = preset(0xffdeb887, "burly wood")

  /** <span style="background:#5f9ea0;border:1px solid gray">&emsp;&emsp;</span> `#5f9ea0` */
  val CadetBlue: Color = preset(0xff5f9ea0, "cadet blue")

  /** <span style="background:#7fff00;border:1px solid gray">&emsp;&emsp;</span> `#7fff00` */
  val Chartreuse: Color = preset(0xff7fff00, "chartreuse")

  /** <span style="background:#d2691e;border:1px solid gray">&emsp;&emsp;</span> `#d2691e` */
  val Chocolate: Color = preset(0xffd2691e, "chocolate")

  /** <span style="background:#ff7f50;border:1px solid gray">&emsp;&emsp;</span> `#ff7f50` */
  val Coral: Color = preset(0xffff7f50, "coral")

  /** <span style="background:#6495ed;border:1px solid gray">&emsp;&emsp;</span> `#6495ed` */
  val CornflowerBlue: Color = preset(0xff6495ed, "cornflower blue")

  /** <span style="background:#fff8dc;border:1px solid gray">&emsp;&emsp;</span> `#fff8dc` */
  val CornSilk: Color = preset(0xfffff8dc, "cornsilk")

  /** <span style="background:#dc143c;border:1px solid gray">&emsp;&emsp;</span> `#dc143c` */
  val Crimson: Color = preset(0xffdc143c, "crimson")

  /** <span style="background:#00ffff;border:1px solid gray">&emsp;&emsp;</span> `#00ffff` */
  val Cyan: Color = preset(0xff00ffff, "cyan")

  /** <span style="background:#00008b;border:1px solid gray">&emsp;&emsp;</span> `#00008b` */
  val DarkBlue: Color = preset(0xff00008b, "dark blue")

  /** <span style="background:#008b8b;border:1px solid gray">&emsp;&emsp;</span> `#008b8b` */
  val DarkCyan: Color = preset(0xff008b8b, "dark cyan")

  /** <span style="background:#b8860b;border:1px solid gray">&emsp;&emsp;</span> `#b8860b` */
  val DarkGoldenrod: Color = preset(0xffb8860b, "dark goldenrod")

  /** <span style="background:#a9a9a9;border:1px solid gray">&emsp;&emsp;</span> `#a9a9a9` */
  val DarkGray: Color = preset(0xffa9a9a9, "dark gray")

  /** <span style="background:#006400;border:1px solid gray">&emsp;&emsp;</span> `#006400` */
  val DarkGreen: Color = preset(0xff006400, "dark green")

  /** <span style="background:#a9a9a9;border:1px solid gray">&emsp;&emsp;</span> `#a9a9a9` */
  val DarkGrey: Color = preset(0xffa9a9a9, "dark grey")

  /** <span style="background:#bdb76b;border:1px solid gray">&emsp;&emsp;</span> `#bdb76b` */
  val DarkKhaki: Color = preset(0xffbdb76b, "dark khaki")

  /** <span style="background:#8b008b;border:1px solid gray">&emsp;&emsp;</span> `#8b008b` */
  val DarkMagenta: Color = preset(0xff8b008b, "dark magenta")

  /** <span style="background:#556b2f;border:1px solid gray">&emsp;&emsp;</span> `#556b2f` */
  val DarkOliveGreen: Color = preset(0xff556b2f, "dark olive green")

  /** <span style="background:#ff8c00;border:1px solid gray">&emsp;&emsp;</span> `#ff8c00` */
  val DarkOrange: Color = preset(0xffff8c00, "dark orange")

  /** <span style="background:#9932cc;border:1px solid gray">&emsp;&emsp;</span> `#9932cc` */
  val DarkOrchid: Color = preset(0xff9932cc, "dark orchid")

  /** <span style="background:#8b0000;border:1px solid gray">&emsp;&emsp;</span> `#8b0000` */
  val DarkRed: Color = preset(0xff8b0000, "dark red")

  /** <span style="background:#e9967a;border:1px solid gray">&emsp;&emsp;</span> `#e9967a` */
  val DarkSalmon: Color = preset(0xffe9967a, "dark salmon")

  /** <span style="background:#8fbc8f;border:1px solid gray">&emsp;&emsp;</span> `#8fbc8f` */
  val DarkSeaGreen: Color = preset(0xff8fbc8f, "dark sea green")

  /** <span style="background:#483d8b;border:1px solid gray">&emsp;&emsp;</span> `#483d8b` */
  val DarkSlateBlue: Color = preset(0xff483d8b, "dark slate blue")

  /** <span style="background:#2f4f4f;border:1px solid gray">&emsp;&emsp;</span> `#2f4f4f` */
  val DarkSlateGray: Color = preset(0xff2f4f4f, "dark slate gray")

  /** <span style="background:#2f4f4f;border:1px solid gray">&emsp;&emsp;</span> `#2f4f4f` */
  val DarkSlateGrey: Color = preset(0xff2f4f4f, "dark slate grey")

  /** <span style="background:#00ced1;border:1px solid gray">&emsp;&emsp;</span> `#00ced1` */
  val DarkTurquoise: Color = preset(0xff00ced1, "dark turquoise")

  /** <span style="background:#9400d3;border:1px solid gray">&emsp;&emsp;</span> `#9400d3` */
  val DarkViolet: Color = preset(0xff9400d3, "dark violet")

  /** <span style="background:#ff1493;border:1px solid gray">&emsp;&emsp;</span> `#ff1493` */
  val DeepPink: Color = preset(0xffff1493, "deep pink")

  /** <span style="background:#00bfff;border:1px solid gray">&emsp;&emsp;</span> `#00bfff` */
  val DeepSkyBlue: Color = preset(0xff00bfff, "deep sky blue")

  /** <span style="background:#696969;border:1px solid gray">&emsp;&emsp;</span> `#696969` */
  val DimGray: Color = preset(0xff696969, "dim gray")

  /** <span style="background:#696969;border:1px solid gray">&emsp;&emsp;</span> `#696969` */
  val DimGrey: Color = preset(0xff696969, "dim grey")

  /** <span style="background:#1e90ff;border:1px solid gray">&emsp;&emsp;</span> `#1e90ff` */
  val DodgerBlue: Color = preset(0xff1e90ff, "dodger blue")

  /** <span style="background:#b22222;border:1px solid gray">&emsp;&emsp;</span> `#b22222` */
  val FireBrick: Color = preset(0xffb22222, "fire brick")

  /** <span style="background:#fffaf0;border:1px solid gray">&emsp;&emsp;</span> `#fffaf0` */
  val FloralWhite: Color = preset(0xfffffaf0, "floral white")

  /** <span style="background:#228b22;border:1px solid gray">&emsp;&emsp;</span> `#228b22` */
  val ForestGreen: Color = preset(0xff228b22, "forest green")

  /** <span style="background:#ff00ff;border:1px solid gray">&emsp;&emsp;</span> `#ff00ff` */
  val Fuchsia: Color = preset(0xffff00ff, "fuchsia")

  /** <span style="background:#dcdcdc;border:1px solid gray">&emsp;&emsp;</span> `#dcdcdc` */
  val Gainsboro: Color = preset(0xffdcdcdc, "gainsboro")

  /** <span style="background:#f8f8ff;border:1px solid gray">&emsp;&emsp;</span> `#f8f8ff` */
  val GhostWhite: Color = preset(0xfff8f8ff, "ghost white")

  /** <span style="background:#ffd700;border:1px solid gray">&emsp;&emsp;</span> `#ffd700` */
  val Gold: Color = preset(0xffffd700, "gold")

  /** <span style="background:#daa520;border:1px solid gray">&emsp;&emsp;</span> `#daa520` */
  val Goldenrod: Color = preset(0xffdaa520, "goldenrod")

  /** <span style="background:#808080;border:1px solid gray">&emsp;&emsp;</span> `#808080` */
  val Gray: Color = preset(0xff808080, "gray")

  /** <span style="background:#008000;border:1px solid gray">&emsp;&emsp;</span> `#008000` */
  val Green: Color = preset(0xff008000, "green")

  /** <span style="background:#adff2f;border:1px solid gray">&emsp;&emsp;</span> `#adff2f` */
  val GreenYellow: Color = preset(0xffadff2f, "green yellow")

  /** <span style="background:#808080;border:1px solid gray">&emsp;&emsp;</span> `#808080` */
  val Grey: Color = preset(0xff808080, "grey")

  /** <span style="background:#f0fff0;border:1px solid gray">&emsp;&emsp;</span> `#f0fff0` */
  val Honeydew: Color = preset(0xfff0fff0, "honeydew")

  /** <span style="background:#ff69b4;border:1px solid gray">&emsp;&emsp;</span> `#ff69b4` */
  val HotPink: Color = preset(0xffff69b4, "hot pink")

  /** <span style="background:#cd5c5c;border:1px solid gray">&emsp;&emsp;</span> `#cd5c5c` */
  val IndianRed: Color = preset(0xffcd5c5c, "indian red")

  /** <span style="background:#4b0082;border:1px solid gray">&emsp;&emsp;</span> `#4b0082` */
  val Indigo: Color = preset(0xff4b0082, "indigo")

  /** <span style="background:#fffff0;border:1px solid gray">&emsp;&emsp;</span> `#fffff0` */
  val Ivory: Color = preset(0xfffffff0, "ivory")

  /** <span style="background:#f0e68c;border:1px solid gray">&emsp;&emsp;</span> `#f0e68c` */
  val Khaki: Color = preset(0xfff0e68c, "khaki")

  /** <span style="background:#e6e6fa;border:1px solid gray">&emsp;&emsp;</span> `#e6e6fa` */
  val Lavender: Color = preset(0xffe6e6fa, "lavender")

  /** <span style="background:#fff0f5;border:1px solid gray">&emsp;&emsp;</span> `#fff0f5` */
  val LavenderBlush: Color = preset(0xfffff0f5, "lavender blush")

  /** <span style="background:#7cfc00;border:1px solid gray">&emsp;&emsp;</span> `#7cfc00` */
  val LawnGreen: Color = preset(0xff7cfc00, "lawn green")

  /** <span style="background:#fffacd;border:1px solid gray">&emsp;&emsp;</span> `#fffacd` */
  val LemonChiffon: Color = preset(0xfffffacd, "lemon chiffon")

  /** <span style="background:#add8e6;border:1px solid gray">&emsp;&emsp;</span> `#add8e6` */
  val LightBlue: Color = preset(0xffadd8e6, "light blue")

  /** <span style="background:#f08080;border:1px solid gray">&emsp;&emsp;</span> `#f08080` */
  val LightCoral: Color = preset(0xfff08080, "light coral")

  /** <span style="background:#e0ffff;border:1px solid gray">&emsp;&emsp;</span> `#e0ffff` */
  val LightCyan: Color = preset(0xffe0ffff, "light cyan")

  /** <span style="background:#fafad2;border:1px solid gray">&emsp;&emsp;</span> `#fafad2` */
  val LightGoldenrodYellow: Color =
    preset(0xfffafad2, "light goldenrod yellow")

  /** <span style="background:#d3d3d3;border:1px solid gray">&emsp;&emsp;</span> `#d3d3d3` */
  val LightGray: Color = preset(0xffd3d3d3, "light gray")

  /** <span style="background:#90ee90;border:1px solid gray">&emsp;&emsp;</span> `#90ee90` */
  val LightGreen: Color = preset(0xff90ee90, "light green")

  /** <span style="background:#d3d3d3;border:1px solid gray">&emsp;&emsp;</span> `#d3d3d3` */
  val LightGrey: Color = preset(0xffd3d3d3, "light grey")

  /** <span style="background:#ffb6c1;border:1px solid gray">&emsp;&emsp;</span> `#ffb6c1` */
  val LightPink: Color = preset(0xffffb6c1, "light pink")

  /** <span style="background:#ffa07a;border:1px solid gray">&emsp;&emsp;</span> `#ffa07a` */
  val LightSalmon: Color = preset(0xffffa07a, "light salmon")

  /** <span style="background:#20b2aa;border:1px solid gray">&emsp;&emsp;</span> `#20b2aa` */
  val LightSeaGreen: Color = preset(0xff20b2aa, "light sea green")

  /** <span style="background:#87cefa;border:1px solid gray">&emsp;&emsp;</span> `#87cefa` */
  val LightSkyBlue: Color = preset(0xff87cefa, "light sky blue")

  /** <span style="background:#778899;border:1px solid gray">&emsp;&emsp;</span> `#778899` */
  val LightSlateGray: Color = preset(0xff778899, "light slate gray")

  /** <span style="background:#778899;border:1px solid gray">&emsp;&emsp;</span> `#778899` */
  val LightSlateGrey: Color = preset(0xff778899, "light slate grey")

  /** <span style="background:#b0c4de;border:1px solid gray">&emsp;&emsp;</span> `#b0c4de` */
  val LightSteelBlue: Color = preset(0xffb0c4de, "light steel blue")

  /** <span style="background:#ffffe0;border:1px solid gray">&emsp;&emsp;</span> `#ffffe0` */
  val LightYellow: Color = preset(0xffffffe0, "light yellow")

  /** <span style="background:#00ff00;border:1px solid gray">&emsp;&emsp;</span> `#00ff00` */
  val Lime: Color = preset(0xff00ff00, "lime")

  /** <span style="background:#32cd32;border:1px solid gray">&emsp;&emsp;</span> `#32cd32` */
  val LimeGreen: Color = preset(0xff32cd32, "lime green")

  /** <span style="background:#faf0e6;border:1px solid gray">&emsp;&emsp;</span> `#faf0e6` */
  val Linen: Color = preset(0xfffaf0e6, "linen")

  /** <span style="background:#ff00ff;border:1px solid gray">&emsp;&emsp;</span> `#ff00ff` */
  val Magenta: Color = preset(0xffff00ff, "magenta")

  /** <span style="background:#800000;border:1px solid gray">&emsp;&emsp;</span> `#800000` */
  val Maroon: Color = preset(0xff800000, "maroon")

  /** <span style="background:#66cdaa;border:1px solid gray">&emsp;&emsp;</span> `#66cdaa` */
  val MediumAquamarine: Color = preset(0xff66cdaa, "medium aquamarine")

  /** <span style="background:#0000cd;border:1px solid gray">&emsp;&emsp;</span> `#0000cd` */
  val MediumBlue: Color = preset(0xff0000cd, "medium blue")

  /** <span style="background:#ba55d3;border:1px solid gray">&emsp;&emsp;</span> `#ba55d3` */
  val MediumOrchid: Color = preset(0xffba55d3, "medium orchid")

  /** <span style="background:#9370db;border:1px solid gray">&emsp;&emsp;</span> `#9370db` */
  val MediumPurple: Color = preset(0xff9370db, "medium purple")

  /** <span style="background:#3cb371;border:1px solid gray">&emsp;&emsp;</span> `#3cb371` */
  val MediumSeaGreen: Color = preset(0xff3cb371, "medium sea green")

  /** <span style="background:#7b68ee;border:1px solid gray">&emsp;&emsp;</span> `#7b68ee` */
  val MediumSlateBlue: Color = preset(0xff7b68ee, "medium slate blue")

  /** <span style="background:#00fa9a;border:1px solid gray">&emsp;&emsp;</span> `#00fa9a` */
  val MediumSpringGreen: Color = preset(0xff00fa9a, "medium spring green")

  /** <span style="background:#48d1cc;border:1px solid gray">&emsp;&emsp;</span> `#48d1cc` */
  val MediumTurquoise: Color = preset(0xff48d1cc, "medium turquoise")

  /** <span style="background:#c71585;border:1px solid gray">&emsp;&emsp;</span> `#c71585` */
  val MediumVioletRed: Color = preset(0xffc71585, "medium violet red")

  /** <span style="background:#191970;border:1px solid gray">&emsp;&emsp;</span> `#191970` */
  val MidnightBlue: Color = preset(0xff191970, "midnight blue")

  /** <span style="background:#f5fffa;border:1px solid gray">&emsp;&emsp;</span> `#f5fffa` */
  val MintCream: Color = preset(0xfff5fffa, "mint cream")

  /** <span style="background:#ffe4e1;border:1px solid gray">&emsp;&emsp;</span> `#ffe4e1` */
  val MistyRose: Color = preset(0xffffe4e1, "misty rose")

  /** <span style="background:#ffe4b5;border:1px solid gray">&emsp;&emsp;</span> `#ffe4b5` */
  val Moccasin: Color = preset(0xffffe4b5, "moccasin")

  /** <span style="background:#ffdead;border:1px solid gray">&emsp;&emsp;</span> `#ffdead` */
  val NavajoWhite: Color = preset(0xffffdead, "navajo white")

  /** <span style="background:#000080;border:1px solid gray">&emsp;&emsp;</span> `#000080` */
  val Navy: Color = preset(0xff000080, "navy")

  /** <span style="background:#fdf5e6;border:1px solid gray">&emsp;&emsp;</span> `#fdf5e6` */
  val OldLace: Color = preset(0xfffdf5e6, "old lace")

  /** <span style="background:#808000;border:1px solid gray">&emsp;&emsp;</span> `#808000` */
  val Olive: Color = preset(0xff808000, "olive")

  /** <span style="background:#6b8e23;border:1px solid gray">&emsp;&emsp;</span> `#6b8e23` */
  val OliveDrab: Color = preset(0xff6b8e23, "olive drab")

  /** <span style="background:#ffa500;border:1px solid gray">&emsp;&emsp;</span> `#ffa500` */
  val Orange: Color = preset(0xffffa500, "orange")

  /** <span style="background:#ff4500;border:1px solid gray">&emsp;&emsp;</span> `#ff4500` */
  val OrangeRed: Color = preset(0xffff4500, "orange red")

  /** <span style="background:#da70d6;border:1px solid gray">&emsp;&emsp;</span> `#da70d6` */
  val Orchid: Color = preset(0xffda70d6, "orchid")

  /** <span style="background:#eee8aa;border:1px solid gray">&emsp;&emsp;</span> `#eee8aa` */
  val PaleGoldenrod: Color = preset(0xffeee8aa, "pale goldenrod")

  /** <span style="background:#98fb98;border:1px solid gray">&emsp;&emsp;</span> `#98fb98` */
  val PaleGreen: Color = preset(0xff98fb98, "pale green")

  /** <span style="background:#afeeee;border:1px solid gray">&emsp;&emsp;</span> `#afeeee` */
  val PaleTurquoise: Color = preset(0xffafeeee, "pale turquoise")

  /** <span style="background:#db7093;border:1px solid gray">&emsp;&emsp;</span> `#db7093` */
  val PaleVioletRed: Color = preset(0xffdb7093, "pale violet red")

  /** <span style="background:#ffefd5;border:1px solid gray">&emsp;&emsp;</span> `#ffefd5` */
  val PapayaWhip: Color = preset(0xffffefd5, "papaya whip")

  /** <span style="background:#ffdab9;border:1px solid gray">&emsp;&emsp;</span> `#ffdab9` */
  val PeachPuff: Color = preset(0xffffdab9, "peach puff")

  /** <span style="background:#cd853f;border:1px solid gray">&emsp;&emsp;</span> `#cd853f` */
  val Peru: Color = preset(0xffcd853f, "peru")

  /** <span style="background:#ffc0cb;border:1px solid gray">&emsp;&emsp;</span> `#ffc0cb` */
  val Pink: Color = preset(0xffffc0cb, "pink")

  /** <span style="background:#dda0dd;border:1px solid gray">&emsp;&emsp;</span> `#dda0dd` */
  val Plum: Color = preset(0xffdda0dd, "plum")

  /** <span style="background:#b0e0e6;border:1px solid gray">&emsp;&emsp;</span> `#b0e0e6` */
  val PowderBlue: Color = preset(0xffb0e0e6, "powder blue")

  /** <span style="background:#800080;border:1px solid gray">&emsp;&emsp;</span> `#800080` */
  val Purple: Color = preset(0xff800080, "purple")

  /** <span style="background:#ff0000;border:1px solid gray">&emsp;&emsp;</span> `#ff0000` */
  val Red: Color = preset(0xffff0000, "red")

  /** <span style="background:#bc8f8f;border:1px solid gray">&emsp;&emsp;</span> `#bc8f8f` */
  val RosyBrown: Color = preset(0xffbc8f8f, "rosy brown")

  /** <span style="background:#4169e1;border:1px solid gray">&emsp;&emsp;</span> `#4169e1` */
  val RoyalBlue: Color = preset(0xff4169e1, "royal blue")

  /** <span style="background:#8b4513;border:1px solid gray">&emsp;&emsp;</span> `#8b4513` */
  val SaddleBrown: Color = preset(0xff8b4513, "saddle brown")

  /** <span style="background:#fa8072;border:1px solid gray">&emsp;&emsp;</span> `#fa8072` */
  val Salmon: Color = preset(0xfffa8072, "salmon")

  /** <span style="background:#f4a460;border:1px solid gray">&emsp;&emsp;</span> `#f4a460` */
  val SandyBrown: Color = preset(0xfff4a460, "sandy brown")

  /** <span style="background:#2e8b57;border:1px solid gray">&emsp;&emsp;</span> `#2e8b57` */
  val SeaGreen: Color = preset(0xff2e8b57, "sea green")

  /** <span style="background:#fff5ee;border:1px solid gray">&emsp;&emsp;</span> `#fff5ee` */
  val SeaShell: Color = preset(0xfffff5ee, "seashell")

  /** <span style="background:#a0522d;border:1px solid gray">&emsp;&emsp;</span> `#a0522d` */
  val Sienna: Color = preset(0xffa0522d, "sienna")

  /** <span style="background:#c0c0c0;border:1px solid gray">&emsp;&emsp;</span> `#c0c0c0` */
  val Silver: Color = preset(0xffc0c0c0, "silver")

  /** <span style="background:#87ceeb;border:1px solid gray">&emsp;&emsp;</span> `#87ceeb` */
  val SkyBlue: Color = preset(0xff87ceeb, "sky blue")

  /** <span style="background:#6a5acd;border:1px solid gray">&emsp;&emsp;</span> `#6a5acd` */
  val SlateBlue: Color = preset(0xff6a5acd, "slate blue")

  /** <span style="background:#708090;border:1px solid gray">&emsp;&emsp;</span> `#708090` */
  val SlateGray: Color = preset(0xff708090, "slate gray")

  /** <span style="background:#708090;border:1px solid gray">&emsp;&emsp;</span> `#708090` */
  val SlateGrey: Color = preset(0xff708090, "slate grey")

  /** <span style="background:#fffafa;border:1px solid gray">&emsp;&emsp;</span> `#fffafa` */
  val Snow: Color = preset(0xfffffafa, "snow")

  /** <span style="background:#00ff7f;border:1px solid gray">&emsp;&emsp;</span> `#00ff7f` */
  val SpringGreen: Color = preset(0xff00ff7f, "spring green")

  /** <span style="background:#4682b4;border:1px solid gray">&emsp;&emsp;</span> `#4682b4` */
  val SteelBlue: Color = preset(0xff4682b4, "steel blue")

  /** <span style="background:#d2b48c;border:1px solid gray">&emsp;&emsp;</span> `#d2b48c` */
  val Tan: Color = preset(0xffd2b48c, "tan")

  /** <span style="background:#008080;border:1px solid gray">&emsp;&emsp;</span> `#008080` */
  val Teal: Color = preset(0xff008080, "teal")

  /** <span style="background:#d8bfd8;border:1px solid gray">&emsp;&emsp;</span> `#d8bfd8` */
  val Thistle: Color = preset(0xffd8bfd8, "thistle")

  /** <span style="background:#ff6347;border:1px solid gray">&emsp;&emsp;</span> `#ff6347` */
  val Tomato: Color = preset(0xffff6347, "tomato")

  /** <span style="background:#40e0d0;border:1px solid gray">&emsp;&emsp;</span> `#40e0d0` */
  val Turquoise: Color = preset(0xff40e0d0, "turquoise")

  /** <span style="background:#ee82ee;border:1px solid gray">&emsp;&emsp;</span> `#ee82ee` */
  val Violet: Color = preset(0xffee82ee, "violet")

  /** <span style="background:#f5deb3;border:1px solid gray">&emsp;&emsp;</span> `#f5deb3` */
  val Wheat: Color = preset(0xfff5deb3, "wheat")

  /** <span style="background:#ffffff;border:1px solid gray">&emsp;&emsp;</span> `#ffffff` */
  val White: Color = preset(0xffffffff, "white")

  /** <span style="background:#f5f5f5;border:1px solid gray">&emsp;&emsp;</span> `#f5f5f5` */
  val WhiteSmoke: Color = preset(0xfff5f5f5, "white smoke")

  /** <span style="background:#ffff00;border:1px solid gray">&emsp;&emsp;</span> `#ffff00` */
  val Yellow: Color = preset(0xffffff00, "yellow")

  /** <span style="background:#9acd32;border:1px solid gray">&emsp;&emsp;</span> `#9acd32` */
  val YellowGreen: Color = preset(0xff9acd32, "yellow green")

  /** <span style="background:#319400;border:1px solid gray">&emsp;&emsp;</span> `#319400` */
  val AfricanGreen: Color = preset(Color.argbIntFrom(49, 148, 0, 255), "African green")

  /** <span style="background:#952038;border:1px solid gray">&emsp;&emsp;</span> `#952038` */
  val AfricanRed: Color = preset(Color.argbIntFrom(149, 32, 56, 255), "African red")

  /** <span style="background:#f7c608;border:1px solid gray">&emsp;&emsp;</span> `#f7c608` */
  val AfricanYellow: Color =
    preset(Color.argbIntFrom(247, 198, 8, 255), "African yellow")

  /** <span style="background:#3c3b6e;border:1px solid gray">&emsp;&emsp;</span> `#3c3b6e` */
  val AmericanBlue: Color = preset(Color.argbIntFrom(60, 59, 110, 255), "American blue")

  /** <span style="background:#b22234;border:1px solid gray">&emsp;&emsp;</span> `#b22234` */
  val AmericanRed: Color = preset(Color.argbIntFrom(178, 34, 52, 255), "American red")

  /** <span style="background:#00008b;border:1px solid gray">&emsp;&emsp;</span> `#00008b` */
  val AustralianBlue: Color =
    preset(Color.argbIntFrom(0, 0, 139, 255), "Australian blue")

  /** <span style="background:#ff0000;border:1px solid gray">&emsp;&emsp;</span> `#ff0000` */
  val AustralianRed: Color = preset(Color.argbIntFrom(255, 0, 0, 255), "Australian red")

  /** <span style="background:#ed2939;border:1px solid gray">&emsp;&emsp;</span> `#ed2939` */
  val AustrianRed: Color = preset(Color.argbIntFrom(237, 41, 57, 255), "Austrian red")

  /** <span style="background:#ed2939;border:1px solid gray">&emsp;&emsp;</span> `#ed2939` */
  val BelgianRed: Color = preset(Color.argbIntFrom(237, 41, 57, 255), "Belgian red")

  /** <span style="background:#fae042;border:1px solid gray">&emsp;&emsp;</span> `#fae042` */
  val BelgianYellow: Color =
    preset(Color.argbIntFrom(250, 224, 66, 255), "Belgian yellow")

  /** <span style="background:#002776;border:1px solid gray">&emsp;&emsp;</span> `#002776` */
  val BrazilianBlue: Color = preset(Color.argbIntFrom(0, 39, 118, 255), "Brazilian blue")

  /** <span style="background:#009b3a;border:1px solid gray">&emsp;&emsp;</span> `#009b3a` */
  val BrazilianGreen: Color =
    preset(Color.argbIntFrom(0, 155, 58, 255), "Brazilian green")

  /** <span style="background:#00247d;border:1px solid gray">&emsp;&emsp;</span> `#00247d` */
  val BritishBlue: Color = preset(Color.argbIntFrom(0, 36, 125, 255), "British blue")

  /** <span style="background:#cf142b;border:1px solid gray">&emsp;&emsp;</span> `#cf142b` */
  val BritishRed: Color = preset(Color.argbIntFrom(207, 20, 43, 255), "British red")

  /** <span style="background:#fedf00;border:1px solid gray">&emsp;&emsp;</span> `#fedf00` */
  val BrazilianYellow: Color =
    preset(Color.argbIntFrom(254, 223, 0, 255), "Brazilian yellow")

  /** <span style="background:#ff0000;border:1px solid gray">&emsp;&emsp;</span> `#ff0000` */
  val CanadianRed: Color = preset(Color.argbIntFrom(255, 0, 0, 255), "Canadian red")

  /** <span style="background:#000095;border:1px solid gray">&emsp;&emsp;</span> `#000095` */
  val ChineseBlue: Color = preset(Color.argbIntFrom(0, 0, 149, 255), "Chinese blue")

  /** <span style="background:#fe0000;border:1px solid gray">&emsp;&emsp;</span> `#fe0000` */
  val ChineseRed: Color = preset(Color.argbIntFrom(254, 0, 0, 255), "Chinese red")

  /** <span style="background:#11457e;border:1px solid gray">&emsp;&emsp;</span> `#11457e` */
  val CzechBlue: Color = preset(Color.argbIntFrom(17, 69, 126, 255), "Czech blue")

  /** <span style="background:#d7141a;border:1px solid gray">&emsp;&emsp;</span> `#d7141a` */
  val CzechRed: Color = preset(Color.argbIntFrom(215, 20, 26, 255), "Czech red")

  /** <span style="background:#003399;border:1px solid gray">&emsp;&emsp;</span> `#003399` */
  val EuropeanBlue: Color = preset(Color.argbIntFrom(0, 51, 153, 255), "European blue")

  /** <span style="background:#ffcc00;border:1px solid gray">&emsp;&emsp;</span> `#ffcc00` */
  val EuropeanYellow: Color =
    preset(Color.argbIntFrom(255, 204, 0, 255), "European yellow")

  /** <span style="background:#c60c30;border:1px solid gray">&emsp;&emsp;</span> `#c60c30` */
  val DanishRed: Color = preset(Color.argbIntFrom(198, 12, 48, 255), "Danish red")

  /** <span style="background:#003f87;border:1px solid gray">&emsp;&emsp;</span> `#003f87` */
  val FinnishBlue: Color = preset(Color.argbIntFrom(0, 63, 135, 255), "Finnish blue")

  /** <span style="background:#002395;border:1px solid gray">&emsp;&emsp;</span> `#002395` */
  val FrenchBlue: Color = preset(Color.argbIntFrom(0, 35, 149, 255), "French blue")

  /** <span style="background:#ed2939;border:1px solid gray">&emsp;&emsp;</span> `#ed2939` */
  val FrenchRed: Color = preset(Color.argbIntFrom(237, 41, 57, 255), "French red")

  /** <span style="background:#dd0000;border:1px solid gray">&emsp;&emsp;</span> `#dd0000` */
  val GermanRed: Color = preset(Color.argbIntFrom(221, 0, 0, 255), "German red")

  /** <span style="background:#ffce00;border:1px solid gray">&emsp;&emsp;</span> `#ffce00` */
  val GermanYellow: Color = preset(Color.argbIntFrom(255, 206, 0, 255), "German yellow")

  /** <span style="background:#0d5eaf;border:1px solid gray">&emsp;&emsp;</span> `#0d5eaf` */
  val GreekBlue: Color = preset(Color.argbIntFrom(13, 94, 175, 255), "Greek blue")

  /** <span style="background:#c00c30;border:1px solid gray">&emsp;&emsp;</span> `#c00c30` */
  val GreenlandicRed: Color =
    preset(Color.argbIntFrom(192, 12, 48, 255), "Greenlandic red")

  /** <span style="background:#436f4d;border:1px solid gray">&emsp;&emsp;</span> `#436f4d` */
  val HungarianGreen: Color =
    preset(Color.argbIntFrom(67, 111, 77, 255), "Hungarian green")

  /** <span style="background:#cd2a3e;border:1px solid gray">&emsp;&emsp;</span> `#cd2a3e` */
  val HungarianRed: Color = preset(Color.argbIntFrom(205, 42, 62, 255), "Hungarian red")

  /** <span style="background:#003897;border:1px solid gray">&emsp;&emsp;</span> `#003897` */
  val IcelandicBlue: Color = preset(Color.argbIntFrom(0, 56, 151, 255), "Icelandic blue")

  /** <span style="background:#d72828;border:1px solid gray">&emsp;&emsp;</span> `#d72828` */
  val IcelandicRed: Color = preset(Color.argbIntFrom(215, 40, 40, 255), "Icelandic red")

  /** <span style="background:#169b62;border:1px solid gray">&emsp;&emsp;</span> `#169b62` */
  val IrishGreen: Color = preset(Color.argbIntFrom(22, 155, 98, 255), "Irish green")

  /** <span style="background:#ff883e;border:1px solid gray">&emsp;&emsp;</span> `#ff883e` */
  val IrishOrange: Color = preset(Color.argbIntFrom(255, 136, 62, 255), "Irish orange")

  /** <span style="background:#009246;border:1px solid gray">&emsp;&emsp;</span> `#009246` */
  val ItalianGreen: Color = preset(Color.argbIntFrom(0, 146, 70, 255), "Italian green")

  /** <span style="background:#ce2b37;border:1px solid gray">&emsp;&emsp;</span> `#ce2b37` */
  val ItalianRed: Color = preset(Color.argbIntFrom(206, 43, 55, 255), "Italian red")

  /** <span style="background:#bc002d;border:1px solid gray">&emsp;&emsp;</span> `#bc002d` */
  val JapaneseRed: Color = preset(Color.argbIntFrom(188, 0, 45, 255), "Japanese red")

  /** <span style="background:#002868;border:1px solid gray">&emsp;&emsp;</span> `#002868` */
  val NorwegianBlue: Color = preset(Color.argbIntFrom(0, 40, 104, 255), "Norwegian blue")

  /** <span style="background:#ef2b2d;border:1px solid gray">&emsp;&emsp;</span> `#ef2b2d` */
  val NorwegianRed: Color = preset(Color.argbIntFrom(239, 43, 45, 255), "Norwegian red")

  /** <span style="background:#002b7f;border:1px solid gray">&emsp;&emsp;</span> `#002b7f` */
  val RomanianBlue: Color = preset(Color.argbIntFrom(0, 43, 127, 255), "Romanian blue")

  /** <span style="background:#ce1126;border:1px solid gray">&emsp;&emsp;</span> `#ce1126` */
  val RomanianRed: Color = preset(Color.argbIntFrom(206, 17, 38, 255), "Romanian red")

  /** <span style="background:#fcd116;border:1px solid gray">&emsp;&emsp;</span> `#fcd116` */
  val RomanianYellow: Color =
    preset(Color.argbIntFrom(252, 209, 22, 255), "Romanian yellow")

  /** <span style="background:#0039a6;border:1px solid gray">&emsp;&emsp;</span> `#0039a6` */
  val RussianBlue: Color = preset(Color.argbIntFrom(0, 57, 166, 255), "Russian blue")

  /** <span style="background:#d52b1e;border:1px solid gray">&emsp;&emsp;</span> `#d52b1e` */
  val RussianRed: Color = preset(Color.argbIntFrom(213, 43, 30, 255), "Russian red")

  /** <span style="background:#4189dd;border:1px solid gray">&emsp;&emsp;</span> `#4189dd` */
  val SomaliBlue: Color = preset(Color.argbIntFrom(65, 137, 221, 255), "Somali blue")

  /** <span style="background:#006ba8;border:1px solid gray">&emsp;&emsp;</span> `#006ba8` */
  val SwedishBlue: Color = preset(Color.argbIntFrom(0, 107, 168, 255), "Swedish blue")

  /** <span style="background:#fecd01;border:1px solid gray">&emsp;&emsp;</span> `#fecd01` */
  val SwedishYellow: Color =
    preset(Color.argbIntFrom(254, 205, 1, 255), "Swedish yellow")

  /** <span style="background:#da251d;border:1px solid gray">&emsp;&emsp;</span> `#da251d` */
  val VietnameseRed: Color =
    preset(Color.argbIntFrom(218, 37, 29, 255), "Vietnamese red")

  /** <span style="background:#ffff00;border:1px solid gray">&emsp;&emsp;</span> `#ffff00` */
  val VietnameseYellow: Color =
    preset(Color.argbIntFrom(255, 255, 0, 255), "Vietnamese yellow")

  /** <span style="background:#e6e6e6;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 10-percent black. `#e6e6e6`
    */
  val Black10: Color = preset(Black.tintByPercentage(90), "black10")

  /** <span style="background:#cccccc;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 20-percent black. `#cccccc`
    */
  val Black20: Color = preset(Black.tintByPercentage(80), "black20")

  /** <span style="background:#b3b3b3;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 30-percent black. `#b3b3b3`
    */
  val Black30: Color = preset(Black.tintByPercentage(70), "black30")

  /** <span style="background:#999999;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 40-percent black. `#999999`
    */
  val Black40: Color = preset(Black.tintByPercentage(60), "black40")

  /** <span style="background:#808080;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 50-percent black. `#808080`
    */
  val Black50: Color = preset(Black.tintByPercentage(50), "black50")

  /** <span style="background:#666666;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 60-percent black. `#666666`
    */
  val Black60: Color = preset(Black.tintByPercentage(40), "black60")

  /** <span style="background:#4d4d4d;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 70-percent black. `#4d4d4d`
    */
  val Black70: Color = preset(Black.tintByPercentage(30), "black70")

  /** <span style="background:#333333;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 80-percent black. `#333333`
    */
  val Black80: Color = preset(Black.tintByPercentage(20), "black80")

  /** <span style="background:#1a1a1a;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 90-percent black. `#1a1a1a`
    */
  val Black90: Color = preset(Black.tintByPercentage(10), "black90")

  /** <span style="background:#191919;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 10-percent white. `#191919`
    */
  val White10: Color = preset(White.shadeByPercentage(90), "white10")

  /** <span style="background:#323232;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 20-percent white. `#323232`
    */
  val White20: Color = preset(White.shadeByPercentage(80), "white20")

  /** <span style="background:#4c4c4c;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 30-percent white. `#4c4c4c`
    */
  val White30: Color = preset(White.shadeByPercentage(70), "white30")

  /** <span style="background:#666666;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 40-percent white. `#666666`
    */
  val White40: Color = preset(White.shadeByPercentage(60), "white40")

  /** <span style="background:#7f7f7f;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 50-percent white. `#7f7f7f`
    */
  val White50: Color = preset(White.shadeByPercentage(50), "white50")

  /** <span style="background:#999999;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 60-percent white. `#999999`
    */
  val White60: Color = preset(White.shadeByPercentage(40), "white60")

  /** <span style="background:#b2b2b2;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 70-percent white. `#b2b2b2`
    */
  val White70: Color = preset(White.shadeByPercentage(30), "white70")

  /** <span style="background:#cccccc;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 80-percent white. `#cccccc`
    */
  val White80: Color = preset(White.shadeByPercentage(20), "white80")

  /** <span style="background:#e5e5e5;border:1px solid gray">&emsp;&emsp;</span> Color constant for
    * 90-percent white. `#e5e5e5`
    */
  val White90: Color = preset(White.shadeByPercentage(10), "white90")

  /** Color constant for transparent <em>"white"</em>. */
  val Transparent: Color = preset(Color.argbIntFrom(255, 255, 255, 0), "transparent")

  /** Every predefined color under its canonical name, in declaration order. */
  val byName: ListMap[String, Color] = registry.result()
