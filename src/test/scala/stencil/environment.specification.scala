package stencil

import org.scalatest.{FreeSpec, FunSuite}
import scala.xml.XML

class EnvironmentSpecification extends FreeSpec {
  "An environment" - {
    "when empty" - {
      "should resolves all expressions and all lookups to None" in {
        assert(Environment.Empty.resolve("Kalle") === None)
        assert(Environment.Empty.lookup("Kalle") === None)
      }
      "should resolve quoted literals to strings" in {
        assert(Environment.Empty.resolve("'Kalle'") === Some("Kalle"))
      }
    }
    "when consisting of a map" - {
      "should resolve simple expressions to their bound value" in {
        assert(Environment("name" → "Kalle").resolve("name") === Some("Kalle"))
        assert(Environment("name" → "Kalle")("age" -> "30").resolve("name") === Some("Kalle"))
      }
      "should resolve simple expressions to their bound value in the nearest enclosing environment" in {
        assert(Environment("name" → "Kalle").resolve("name") === Some("Kalle"))
        assert(Environment("name" → "Kalle")("name" -> "Nisse").resolve("name") === Some("Nisse"))
      }
      "should resolve complex expressions to their bound value" in {
        assert(Environment("name.first" → "Kalle").resolve("name.first") === Some("Kalle"))
        assert(Environment("name.first" → "Kalle")("age" -> "30").resolve("name.first") === Some("Kalle"))
      }
      "should resolve complex expressions to their bound value in the nearest enclosing environment" in {
        assert(Environment("name.first" → "Kalle").resolve("name.first") === Some("Kalle"))
        assert(Environment("name.first" → "Kalle")("name.first" -> "Nisse").resolve("name.first") === Some("Nisse"))
      }
    }
    "when consisting of an instance" - {
      case class Name(first: String, last: String)
      case class Person(name: Name, alias: Option[String] = None, friends: List[Person] = Nil, properties: Map[String, String] = Map.empty)

      "should resolve missing method expressions to None" in {
        val kalle = Person(Name("Kalle", "Blomkvist"))
        assert(Environment("person" → kalle).resolve("person.alias") === None)
      }
      "should resolve method expressions to their bound value" in {
        val kalle = Person(Name("Kalle", "Blomkvist"))
        assert(Environment("person" → kalle).resolve("person.name.last") === Some("Blomkvist"))
      }
      "should resolve method expressions to their bound value in the nearest enclosing environment" in {
        val kalle = Person(Name("Kalle", "Blomkvist"))
        val nisse = Person(Name("Nisse", "Karlsson"))
        assert(Environment("person" → kalle).resolve("person.name.first") === Some("Kalle"))
        assert(Environment("person" → kalle)("person" -> nisse).resolve("person.name.first") === Some("Nisse"))
      }
      "should resolve method expressions one level at a time" in {
        val kalle = Person(Name("Kalle", "Blomkvist"))
        val nisse = Person(Name("Nisse", "Karlsson"))
        val env = Environment("person" → kalle)("person" -> nisse)("person.name.first" -> "Pelle")
        assert(env.resolve("person.name.first") === Some("Pelle"))
        assert(env.resolve("person.name.last") === Some("Karlsson"))
      }
      "should resolve optional values transparently" in {
        val kalle = Person(Name("Kalle", "Blomkvist"), alias = Some("Detective"))
        assert(Environment("person" → kalle).resolve("person.alias") === Some("Detective"))
      }
      "should resolve optional values once if Some and nonce if None" in {
        val kalle = Person(Name("Kalle", "Blomkvist"), alias = Some("Detective"))
        val nisse = Person(Name("Nisse", "Karlsson"))
        assert(Environment("person" → kalle)("person.alias").size === 1)
        assert(Environment("person" → kalle)("alias", "person.alias").head.resolve("alias") === Some("Detective"))
        assert(Environment("person" → nisse)("person.alias").size === 0)
      }
      "should resolve map values transparently" in {
        val kalle = Person(name = Name("Kalle", "Blomkvist"), properties = Map("age" → "30"))
        assert(Environment("person" → kalle).resolve("person.properties.age") === Some("30"))
        assert(Environment("person" → kalle).resolve("person.properties.x") === None)
      }
      "should traverse traversable values" in {
        val nisse = Person(Name("Nisse", "Karlsson"), alias = Some("Slick"))
        val pelle = Person(Name("Pelle", "Persson"), alias = Some("Animal"))
        val kalle = Person(name = Name("Kalle", "Blomkvist"), friends = nisse :: pelle :: Nil)
        val env = Environment("person" → kalle)
        assert(env("person.friends").size === 2)
        assert(env("friend", "person.friends").map(_.resolve("friend.alias")) === Seq(Some("Slick"), Some("Animal")))
      }
    }

    "when consisting of XML nodes" - {
      val xml = XML.loadString(
        """<person id="kalle">
          | <name first="Kalle" last="Blomkvist"/>
          | <friends>
          |   <person id="nisse" alias="Slick">
          |     <name first="Nisse" last="Karlsson"/>
          |   </person>
          |   <person id="pelle" alias="Animal">
          |     <name first="Pelle" last="Persson"/>
          |   </person>
          | </friends>
          |</person>""")

      "should resolve missing expressions to None" in {
        assert(Environment(xml).resolve("person.alias") === None)
      }
      "should resolve method expressions to their bound value in the nearest enclosing environment" in {
        assert(Environment(xml).resolve("person.name.first") === Some("Kalle"))
        val friends = Environment(xml)("person.friends").head
        val persons1: Seq[Environment] = friends("person")
        assert(persons1.map(_.resolve("person.name.first")) === Seq(Some("Nisse"), Some("Pelle")))
        val persons2: Seq[Environment] = Environment(xml)("person.friends.person")
        assert(persons2.map(_.resolve("person.name.first")) === Seq(Some("Nisse"), Some("Pelle")))
        //val persons3: Seq[Environment] = Environment(xml)("person.friends.*")
        //assert(persons3.map(_.resolve("person.name.first")) === Seq(Some("Nisse"), Some("Pelle")))
      }
      "should resolve method expressions one level at a time" in {
        /*
        val kalle = Person(Name("Kalle", "Blomkvist"))
        val nisse = Person(Name("Nisse", "Karlsson"))
        val env = Environment("person" → kalle)("person" -> nisse)("person.name.first" -> "Pelle")
        assert(env.resolve("person.name.first") === Some("Pelle"))
        assert(env.resolve("person.name.last") === Some("Karlsson"))

        val traverse = env.traverse("person.friends.person")
        assert(traverse.size === 2)
        val traverseFriend = env.traverse("friend", "person.friends.person")
        assert(traverseFriend.map(_.resolve("friend.alias")) === Seq(Some("Slick"), Some("Animal")))
        */
      }
    }
  }
}