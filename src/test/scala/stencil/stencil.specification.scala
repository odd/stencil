package stencil

import org.scalatest.FunSuite

class StencilSpecification extends FunSuite {
  test("empty stencil should result in empty string") {
    assert("", "")
  }
  test("literals should result in themselves") {
    assert("<name>Kalle</name>", "<name>Kalle</name>")
  }
  test("let directives should bind within thier element") {
    assert("""
      <span x:let-name="'Pelle'">
        <name x:set="name">Kalle</name>
      </span>""", """
      <span>
        <name>Pelle</name>
      </span>""")
  }
  test("set directives should replace their target attribute") {
    assert("""
      <span x:let-name="'Pelle'">
        <person x:set-name="name" name="Kalle" />
      </span>""", """
      <span>
        <person name="Pelle"/>
      </span>""")
  }
  test("set body directives should replace their target body") {
    assert("""
      <span x:let-name="'Pelle'">
        <person x:set="name">
          <name>Kalle</name>
        </person>
      </span>""", """
      <span>
        <person>Pelle</person>
      </span>""")
  }
  test("do directives should repeat their target element zero times for empty values") {
    assert("""
      <span x:do="">
        <person>
          <name>Kalle</name>
        </person>
      </span>""", """
      """)
    assert("""
      <span x:do-x="" x="y">
        <person>
          <name>Kalle</name>
        </person>
      </span>""", """
      """)
  }
  test("do directives should repeat their target element zero times for missing values") {
    assert("""
      <span x:do="missing">
        <person>
          <name>Kalle</name>
        </person>
      </span>""", """
      """)
    assert("""
      <span x:do-x="missing" x="y">
        <person>
          <name>Kalle</name>
        </person>
      </span>""", """
      """)
  }
  test("do directives should repeat their target element zero times for None values") {
    assert("""
      <span x:do="manager">
        <manager>
          <name x:set="manager.name">Nisse</name>
        </manager>
      </span>""", """
      """, "manager" → None)
    assert("""
      <span x:do-boss="manager">
        <manager>
          <name x:set="boss.name">Nisse</name>
        </manager>
      </span>""", """
      """, "manager" → None)
  }
  test("do directives should repeat their target element zero times for Nil values") {
    assert("""
      <span x:do="persons">
        <person>
          <names x:set="persons">Kalle</names>
        </person>
      </span>""", """
      """, "persons" → Nil)
    assert("""
      <span x:do-person="persons">
        <person>
          <name x:set="person.name">Kalle</name>
        </person>
      </span>""", """
      """, "persons" → Nil)
  }
  test("directives in comments are ignored") {
    assert("""
      <!--<span x:set-name="persons" name="Nisse">-->
      <span x:do-person="missing">
        <!--<span x:do-person="missing">-->
        <person>
          <name x:set="person.name">Kalle</name>
        </person>
      </span>""", """
      <!--<span x:set-name="persons" name="Nisse">-->
      """, "persons" → "Pelle")
  }

  private def assert(actual: String, expected: String, bindings: (String, AnyRef)*) {
    assert(Stencil(actual).apply(bindings: _*) === expected)
  }
}