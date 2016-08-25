package stencil

import org.scalatest.{FreeSpec, FunSuite}

import scala.xml.XML

class EnvironmentSpecification extends FreeSpec {
  "An environment" - {
    "when empty" - {
      "should return empty seq for all names" in {
        assert(Environment().resolve("Kalle") === None)
        assert(Environment().resolve("Nisse") === None)
      }
      "should resolve quoted literals to strings" in {
        assert(Environment().resolve("'Kalle'") === Some("Kalle"))
      }
      "should resolve conditional operator expressions to positive expression if non empty" in {
        assert(Environment().resolve("'Kalle'?'Pelle':'Nisse'") === Some("Pelle"))
      }
    }
    "when consisting of a binding" - {
      "should resolve simple expressions to their bound value" in {
        assert(Environment("name" → "Kalle").resolve("name") === Some("Kalle"))
        assert(Environment("name" → "Kalle").bind("age" -> "30").resolve("name") === Some("Kalle"))
      }
      "should resolve simple expressions to their bound value in the nearest enclosing environment" in {
        assert(Environment("name" → "Kalle").resolve("name") === Some("Kalle"))
        assert(Environment("name" → "Kalle").bind("name" -> "Nisse").resolve("name") === Some("Nisse"))
      }
      "should resolve complex expressions to their bound value" in {
        assert(Environment("name.first" → "Kalle").traverse("firstName", "name.first").map(_.resolve("firstName")) === Seq(Some("Kalle")))
        assert(Environment("name.first" → "Kalle").bind("age" -> "30").traverse("firstName", "name.first").map(_.resolve("firstName")) === Seq(Some("Kalle")))
      }
      "should resolve complex expressions to their bound value in the nearest enclosing environment with a matching binding" in {
        assert(Environment("name.first" → "Kalle").bind("name", "Nisse").resolve("name.first") === Some("Kalle"))
        assert(Environment("name.first" → "Kalle").bind("name.first", "Nisse").resolve("name.first") === Some("Nisse"))
      }
      "should resolve traversed expressions to their originaly bound value" in {
        assert(Environment("name.first" → "Kalle").bind("firstName" -> "Nisse").traverse("firstName", "name.first").map(_.resolve("firstName")) === Seq(Some("Kalle")))
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
        assert(Environment("person" → kalle).bind("person" -> nisse).resolve("person.name.first") === Some("Nisse"))
      }
      "should resolve method expressions one level at a time" in {
        val kalle = Person(Name("Kalle", "Blomkvist"))
        val nisse = Person(Name("Nisse", "Karlsson"))
        val env = Environment("person" → kalle).bind("person" -> nisse).bind("person.name.first" -> "Pelle")
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
        assert(Environment("person" → kalle).traverse("alias", "person.alias").size === 1)
        assert(Environment("person" → kalle).traverse("alias", "person.alias").map(_.resolve("alias")) === Seq(Some("Detective")))
        assert(Environment("person" → nisse).traverse("alias", "person.alias").size === 0)
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
        val personEnv = Environment("person" → kalle)
        val friendEnvs = personEnv.traverse("friend", "person.friends")
        assert(friendEnvs.size == 2)
        val aliases = friendEnvs.map(_.resolve("friend.alias"))
        assert(aliases === Seq(Some("Slick"), Some("Animal")))
        val aliases2 = personEnv.resolve("person.friends.alias")
        assert(aliases2 === Some(Seq("Slick", "Animal")))
      }
      "should resolve conditional values to correct alternative" in {
        val kalle = Person(name = Name("Kalle", "Blomkvist"), properties = Map("age" → "30"))
        assert(Environment("person" → kalle).resolve("person.properties.age?'known':'unknown'") === Some("known"))
        assert(Environment("person" → kalle).resolve("person.properties.x?'known':'unknown'") === Some("unknown"))
        assert(Environment("person" → kalle).resolve("person.properties.age?:'unknown'") === Some("30"))
        assert(Environment("person" → kalle).resolve("person.properties.x?:'unknown'") === Some("unknown"))
        assert(Environment("person" → kalle).resolve("person.properties.age?person.name.first:'unknown'") === Some("Kalle"))
        assert(Environment("person" → kalle).resolve("person.properties.x?person.name.first:person.name.last") === Some("Blomkvist"))
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
        assert(Environment(xml).traverse("alias", "person.alias") === Seq.empty)
      }
      "should resolve method expressions to their bound value in the nearest enclosing environment" in {
        val env = Environment(xml)
        val friends: Seq[Environment] = env.traverse("friend", "person.friends")
        val friendsHead = friends.head
        val persons1: Seq[Environment] = friendsHead.traverse("person", "friends.person")
        assert(persons1.map(_.resolve("person.name.first")) === Seq(Some("Nisse"), Some("Pelle")))
        val persons2: Seq[Environment] = env.traverse("person", "person.friends.person")
        assert(persons2.map(_.resolve("person.name.first")) === Seq(Some("Nisse"), Some("Pelle")))
        //val persons3: Seq[Environment] = Environment(xml)("person.friends.*")
        //assert(persons3.flatMap(_.resolve("person.name.first")) === Seq(Some("Nisse"), Some("Pelle"))))
      }
      "should resolve method expressions one level at a time" in {
        val env = Environment(xml)
        assert(env.resolve("person.name.first") === Some("Kalle"))
        assert(env.resolve("person.name.last") === Some("Blomkvist"))
        val persons = env.traverse("person", "person.friends.person")
        assert(persons.size === 2)
        assert(persons.map(_.resolve("person.name.first")) === Seq(Some("Nisse"), Some("Pelle")))
      }
    }
  }
}