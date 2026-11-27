package smile.infrastructure

/** How a text's `font` string names a font family on each platform.
  *
  * A family is either a font's own name ("Courier New") or one of CSS's generic families
  * ("monospace"), which a browser resolves to a font of that kind. In CSS a generic family is a
  * keyword and must stay unquoted. Quoted, `"monospace"` names a font that does not exist and the
  * browser falls back to its default.
  */
private[smile] object FontFamily:
  val GenericFamilies: Set[String] = Set(
    "serif",
    "sans-serif",
    "monospace",
    "cursive",
    "fantasy",
    "system-ui",
    "ui-serif",
    "ui-sans-serif",
    "ui-monospace",
    "ui-rounded",
    "math",
    "emoji",
    "fangsong"
  )

  /** Whether a family is a CSS generic family. CSS keywords are case-insensitive. */
  def isGeneric(family: String): Boolean = GenericFamilies.contains(family.trim.toLowerCase)

  /** The family as a CSS `font-family` value. A generic family stays a bare keyword. Any other name
    * is quoted so that names with spaces or digits stay one family.
    */
  def css(family: String): String =
    if isGeneric(family) then family.trim.toLowerCase
    else "\"" + family.replace("\\", "\\\\").replace("\"", "\\\"") + "\""

  /** The family as AWT names it. AWT knows the generic kinds by its own logical font names. A name
    * it does not know at all, such as "monospace", silently becomes Dialog, which is not
    * monospaced. Generic families without an AWT counterpart keep their name and so get Dialog.
    */
  def awt(family: String): String =
    family.trim.toLowerCase match
      case "serif" | "ui-serif"                         => "Serif"
      case "sans-serif" | "ui-sans-serif" | "system-ui" => "SansSerif"
      case "monospace" | "ui-monospace"                 => "Monospaced"
      case _                                            => family
end FontFamily
