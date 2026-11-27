package smile.infrastructure

/** How a text's font string becomes a family on each platform. CSS keeps generic families as
  * keywords, and AWT maps them onto its logical fonts.
  */
class FontFamilySuite extends munit.FunSuite:

  test("CSS keeps generic families unquoted and quotes named fonts") {
    assertEquals(FontFamily.css("monospace"), "monospace")
    assertEquals(FontFamily.css("Sans-Serif"), "sans-serif")
    assertEquals(FontFamily.css("Courier New"), "\"Courier New\"")
    assertEquals(FontFamily.css("Odd \"Name\" \\ x"), "\"Odd \\\"Name\\\" \\\\ x\"")
  }

  test("AWT maps the generic kinds onto its logical fonts and keeps other names") {
    assertEquals(FontFamily.awt("serif"), "Serif")
    assertEquals(FontFamily.awt("sans-serif"), "SansSerif")
    assertEquals(FontFamily.awt("MONOSPACE"), "Monospaced")
    assertEquals(FontFamily.awt("Courier New"), "Courier New")
    assertEquals(FontFamily.awt("SansSerif"), "SansSerif")
  }
end FontFamilySuite
