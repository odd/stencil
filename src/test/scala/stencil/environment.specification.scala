package stencil

import org.scalatest.FunSuite

class EnvironmentSpecification extends FunSuite {
  test("emtpy environments resolves all expressions and all lookups to None") {
    assert(RootEnvironment.resolve("Kalle") === None)
    assert(RootEnvironment.lookup("Kalle") === None)
    assert(RootEnvironment.bind().resolve("Kalle") === None)
    assert(RootEnvironment.bind().lookup("Kalle") === None)
  }
  
  test("Quoted literals resolve to strings") {
    assert(RootEnvironment.resolve("'Kalle'") === Some("Kalle"))
  }

  test("Simple expressions resolve to their bound value") {
    assert(RootEnvironment.bind("name" → "Kalle").resolve("name") === Some("Kalle"))
    assert(RootEnvironment.bind("name" → "Kalle").bind("age" -> "30").resolve("name") === Some("Kalle"))
  }

  test("Simple expressions resolve to their bound value in the nearest enclosing environment") {
    assert(RootEnvironment.bind("name" → "Kalle").resolve("name") === Some("Kalle"))
    assert(RootEnvironment.bind("name" → "Kalle").bind("name" -> "Nisse").resolve("name") === Some("Nisse"))
  }

  test("Complex expressions resolve to their bound value") {
    assert(RootEnvironment.bind("name.first" → "Kalle").resolve("name.first") === Some("Kalle"))
    assert(RootEnvironment.bind("name.first" → "Kalle").bind("age" -> "30").resolve("name.first") === Some("Kalle"))
  }

  test("Complex expressions resolve to their bound value in the nearest enclosing environment") {
    assert(RootEnvironment.bind("name.first" → "Kalle").resolve("name.first") === Some("Kalle"))
    assert(RootEnvironment.bind("name.first" → "Kalle").bind("name.first" -> "Nisse").resolve("name.first") === Some("Nisse"))
  }

  case class Name(first: String, last: String)
  case class Person(name: Name, alias: Option[String] = None, friends: List[Person] = Nil, properties: Map[String, String] = Map.empty)

  test("Missing method expressions are resolved to None") {
    val kalle = Person(Name("Kalle", "Blomkvist"))
    assert(RootEnvironment.bind("person" → kalle).resolve("person.alias") === None)
  }
  test("Method expressions are resolved to their bound value") {
    val kalle = Person(Name("Kalle", "Blomkvist"))
    assert(RootEnvironment.bind("person" → kalle).resolve("person.name.last") === Some("Blomkvist"))
  }
  test("Method expressions are resolved to their bound value in the nearest enclosing environment") {
    val kalle = Person(Name("Kalle", "Blomkvist"))
    val nisse = Person(Name("Nisse", "Karlsson"))
    assert(RootEnvironment.bind("person" → kalle).resolve("person.name.first") === Some("Kalle"))
    assert(RootEnvironment.bind("person" → kalle).bind("person" -> nisse).resolve("person.name.first") === Some("Nisse"))
  }
  test("Method expressions are resolved one level at a time") {
    val kalle = Person(Name("Kalle", "Blomkvist"))
    val nisse = Person(Name("Nisse", "Karlsson"))
    val env = RootEnvironment.bind("person" → kalle).bind("person" -> nisse).bind("person.name.first" -> "Pelle")
    assert(env.resolve("person.name.first") === Some("Pelle"))
    assert(env.resolve("person.name.last") === Some("Karlsson"))
  }
  test("Optional values are resolved transparently") {
    val kalle = Person(Name("Kalle", "Blomkvist"), alias = Some("Detective"))
    assert(RootEnvironment.bind("person" → kalle).resolve("person.alias") === Some("Detective"))
  }
  test("Optional values are traversed once if Some and nonce if None") {
    val kalle = Person(Name("Kalle", "Blomkvist"), alias = Some("Detective"))
    val nisse = Person(Name("Nisse", "Karlsson"))
    assert(RootEnvironment.bind("person" → kalle).traverse("person.alias").size === 1)
    assert(RootEnvironment.bind("person" → kalle).traverse("alias", "person.alias").head.resolve("alias") === Some("Detective"))
    assert(RootEnvironment.bind("person" → nisse).traverse("person.alias").size === 0)
  }
  test("Map values are resolved transparently") {
    val kalle = Person(name = Name("Kalle", "Blomkvist"), properties = Map("age" → "30"))
    assert(RootEnvironment.bind("person" → kalle).resolve("person.properties.age") === Some("30"))
    assert(RootEnvironment.bind("person" → kalle).resolve("person.properties.x") === None)
  }
  test("Traversable values are traversed") {
    val nisse = Person(Name("Nisse", "Karlsson"), alias = Some("Slick"))
    val pelle = Person(Name("Pelle", "Persson"), alias = Some("Animal"))
    val kalle = Person(name = Name("Kalle", "Blomkvist"), friends = nisse :: pelle :: Nil)
    val env = RootEnvironment.bind("person" → kalle)
    assert(env.traverse("person.friends").size === 2)
    assert(env.traverse("friend", "person.friends").map(_.resolve("friend.alias")) === Seq(Some("Slick"), Some("Animal")))
  }
}