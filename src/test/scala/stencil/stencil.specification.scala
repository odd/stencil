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
  test("do directives should repeat their target element zero times for false values") {
    case class Manager(name: String, active: Boolean)
    assert("""
      <prefix/>
      <span x:do="manager.active">
        <manager>
          <name x:set="manager.name">Nisse</name>
        </manager>
      </span>
      <suffix/>""", """
      <prefix/>
      
      <suffix/>""", "manager" → Manager("Pelle", false))
  }
  test("do directives should repeat their target element zero times for None values") {
    assert("""
      <prefix/>
      <span x:do="manager">
        <manager>
          <name x:set="manager.name">Nisse</name>
        </manager>
      </span>
      <suffix/>""", """
      <prefix/>
      
      <suffix/>""", "manager" → None)
    assert("""
      <prefix/>
      <span x:do-boss="manager">
        <manager>
          <name x:set="boss.name">Nisse</name>
        </manager>
      </span>
      <suffix/>""", """
      <prefix/>
      
      <suffix/>""", "manager" → None)
  }
  test("do directives should repeat their target element zero times for Nil values") {
    assert("""
      <prefix/>
      <span x:do="persons">
        <person>
          <name x:set="persons">Kalle</name>
        </person>
      </span>
      <suffix/>""", """
      <prefix/>
      
      <suffix/>""", "persons" → Nil)
    assert("""
      <span x:do-person="persons">
        <person>
          <name x:set="person.name">Kalle</name>
        </person>
      </span>""", """
      """, "persons" → Nil)
  }
  case class Person(name: String, company: String)
  test("do directives should repeat their target element the correct number of times") {
    assert("""
      <prefix/>
      <person x:do-person="persons">
        <name x:set="person.name">Kalle</name>
        <company x:set="person.company">ACME</company>
      </person>
      <suffix/>""", """
      <prefix/>
      <person>
        <name>Lasse</name>
        <company>FOO</company>
      </person><person>
        <name>Pelle</name>
        <company>BAR</company>
      </person><person>
        <name>Nisse</name>
        <company>FOOBAR</company>
      </person>
      <suffix/>""", "persons" → List(Person("Lasse", "FOO"), Person("Pelle", "BAR"), Person("Nisse", "FOOBAR")))
  }
  test("missing end tags should throw exception") {
    intercept[IllegalStateException](Stencil("""
      <prefix/>
      <person x:do-person="persons">
        <name x:set-value="person.name" value="Kalle">
        <company x:set="person.company">ACME</company>
      </person>
      <suffix/>"""))
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
  test("replacement expressions are resolved for set body bindings") {
    assert("""
      <span x:set="/Kalle/person/">Hello Kalle!</span>
      """, """
      <span>Hello Pelle!</span>
      """, "person" → "Pelle")
  }
  test("replacement expressions are resolved for set bindings") {
    assert("""
      <span x:set-title="/Kalle/person/" title="Hello Kalle!"/>
      """, """
      <span title="Hello Pelle!"/>
      """, "person" → "Pelle")
  }
  /*
  test("complex replacement expressions are resolved for set bindings") {
    assert("""
      <span x:set="/Hello/'Grettings'/;/Kalle/person/">Hello Kalle!</span>
      <span x:set="/(Hello)(Kalle)/'Grettings';person/">Hello Kalle!</span>
      """, """
      <span>Greetings Pelle!</span>
      """, "person" → "Pelle")
  }
  */

  private def assert(actual: String, expected: String, bindings: (String, AnyRef)*) {
    assert(Stencil(actual).apply(bindings: _*) === expected)
  }
}