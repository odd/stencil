package stencil

import org.scalatest.{FreeSpec, FunSuite}

import scala.xml.XML

class EnvironmentSpecification extends FreeSpec {
  case class Name(first: String, last: String)
  case class Person(name: Name,
                    alias: Option[String] = None,
                    friends: List[Person] = Nil,
                    properties: Map[String, Any] = Map.empty) {
    def attributes: Map[String, Any] = properties
  }

  def E(value: Any)(implicit accessor: Environment.Accessor): Environment =
    Environment(value)
  def R[T](env: Environment, exp: String): Option[String] = env.resolve(exp).map(_.toString)
  def O[T](value: T): Option[T] = Option(value)

  "An environment" - {
    import Environment.Accessor.default

    "when empty" - {
      "should return empty seq for all names" in {
        assert(E().resolve("Kalle") === None)
        assert(E().resolve("Nisse") === None)
      }
      "should resolve quoted literals to strings" in {
        assert(E().resolve("'Kalle'") === O("Kalle"))
      }
      "should resolve '@' to correct accessor" in {
        val kalle = Person(Name("Kalle", "Blomkvist"))
        assert(
          E().bind("person", kalle).resolve("person@class") === O(
            kalle.getClass.getName
          )
        )
        assert(
          E().bind("person", kalle).resolve("person@kind") === O(
            kalle.getClass.getSimpleName.toLowerCase()
          )
        )
      }
      "should resolve conditional operator expressions to positive expression if non empty" in {
        assert(E().resolve("'Kalle'?'Pelle':'Nisse'") === O("Pelle"))
      }
    }
    "when consisting of a binding" - {
      "should resolve simple expressions to their bound value" in {
        assert(E("name" → "Kalle").resolve("name") === O("Kalle"))
        assert(
          E("name" → "Kalle")
            .bind("age" -> "30")
            .resolve("name") === O("Kalle")
        )
      }
      "should resolve simple expressions to their bound value in the nearest enclosing environment" in {
        assert(E("name" → "Kalle").resolve("name") === O("Kalle"))
        assert(
          E("name" → "Kalle")
            .bind("name" -> "Nisse")
            .resolve("name") === O("Nisse")
        )
      }
      "should resolve complex expressions to their bound value" in {
        assert(
          E("name.first" → "Kalle")
            .traverse("firstName", "name.first")
            .map(_.resolve("firstName")) === Seq(O("Kalle"))
        )
        assert(
          E("name.first" → "Kalle")
            .bind("age" -> "30")
            .traverse("firstName", "name.first")
            .map(_.resolve("firstName")) === Seq(O("Kalle"))
        )
      }
      "should resolve complex expressions to their bound value in the nearest enclosing environment with a matching binding" in {
        assert(
          E("name.first" → "Kalle")
            .bind("name", "Nisse")
            .resolve("name.first") === O("Kalle")
        )
        assert(
          E("name.first" → "Kalle")
            .bind("name.first", "Nisse")
            .resolve("name.first") === O("Nisse")
        )
      }
      "should resolve traversed expressions to their originaly bound value" in {
        assert(
          E("name.first" → "Kalle")
            .bind("firstName" -> "Nisse")
            .traverse("firstName", "name.first")
            .map(_.resolve("firstName")) === Seq(O("Kalle"))
        )
      }
    }
    "when consisting of an instance" - {
      "should resolve missing method expressions to None" in {
        val kalle = Person(Name("Kalle", "Blomkvist"))
        assert(E("person" → kalle).resolve("person.alias") === None)
      }
      "should resolve method expressions to their bound value" in {
        val kalle = Person(Name("Kalle", "Blomkvist"))
        assert(
          E("person" → kalle).resolve("person.name.last") === O("Blomkvist")
        )
      }
      "should resolve method expressions to their bound value in the nearest enclosing environment" in {
        val kalle = Person(Name("Kalle", "Blomkvist"))
        val nisse = Person(Name("Nisse", "Karlsson"))
        assert(E("person" → kalle).resolve("person.name.first") === O("Kalle"))
        assert(
          E("person" → kalle)
            .bind("person" -> nisse)
            .resolve("person.name.first") === O("Nisse")
        )
      }
      "should resolve method expressions one level at a time" in {
        val kalle = Person(Name("Kalle", "Blomkvist"))
        val nisse = Person(Name("Nisse", "Karlsson"))
        val env =
          E("person" → kalle)
            .bind("person" -> nisse)
            .bind("person.name.first" -> "Pelle")
        assert(env.resolve("person.name.first") === O("Pelle"))
        assert(env.resolve("person.name.last") === O("Karlsson"))
      }
      "should resolve optional values transparently" in {
        val kalle =
          Person(Name("Kalle", "Blomkvist"), alias = O("Detective"))
        assert(E("person" → kalle).resolve("person.alias") === O("Detective"))
      }
      "should resolve optional values once if Some and nonce if None" in {
        val kalle =
          Person(Name("Kalle", "Blomkvist"), alias = O("Detective"))
        val nisse = Person(Name("Nisse", "Karlsson"))
        assert(
          E("person" → kalle)
            .traverse("alias", "person.alias")
            .size === 1
        )
        assert(
          E("person" → kalle)
            .traverse("alias", "person.alias")
            .map(_.resolve("alias")) === Seq(O("Detective"))
        )
        assert(
          E("person" → nisse)
            .traverse("alias", "person.alias")
            .size === 0
        )
      }
      "should resolve map values transparently" in {
        val kalle = Person(
          name = Name("Kalle", "Blomkvist"),
          properties = Map("age" → "30")
        )
        assert(
          E("person" → kalle)
            .resolve("person.properties.age") === O("30")
        )
        assert(E("person" → kalle).resolve("person.properties.x") === None)
      }
      "should resolve complex map values transparently" in {
        val kalle = Person(
          name = Name("Kalle", "Blomkvist"),
          properties = Map("company.ceo" → Name("Nisse", "Andersson"))
        )
        assert(
          E("person" → kalle)
            .resolve("person.properties.company.ceo") === O(
            Name("Nisse", "Andersson")
          )
        )
        assert(
          E("person" → kalle)
            .resolve("person.properties.company") === None
        )
      }
      "should resolve map returning methods transparently" in {
        val kalle =
          Person(
            name = Name("Kalle", "Blomkvist"),
            properties = Map("age" → 30)
          )
        assert(
          E("person" → kalle)
            .resolve("person.attributes.age") === O(30)
        )
        assert(E("person" → kalle).resolve("person.attributes.x") === None)
      }
      "should resolve complex map returning methods transparently" in {
        val kalle =
          Person(
            name = Name("Kalle", "Blomkvist"),
            properties = Map("company.ceo" → Name("Nisse", "Andersson"))
          )
        assert(
          E("person" → kalle)
            .resolve("person.attributes.company.ceo") === O(
            Name("Nisse", "Andersson")
          )
        )
        assert(
          E("person" → kalle)
            .resolve("person.attributes.company") === None
        )
      }
      "should traverse traversable values" in {
        val nisse = Person(Name("Nisse", "Karlsson"), alias = O("Slick"))
        val pelle = Person(Name("Pelle", "Persson"), alias = O("Animal"))
        val kalle = Person(
          name = Name("Kalle", "Blomkvist"),
          friends = nisse :: pelle :: Nil
        )
        val personEnv = E("person" → kalle)
        val friendEnvs = personEnv.traverse("friend", "person.friends")
        assert(friendEnvs.size == 2)
        val aliases = friendEnvs.map(_.resolve("friend.alias"))
        assert(aliases === Seq(O("Slick"), O("Animal")))
        val aliases2 = personEnv.resolve("person.friends.alias")
        assert(aliases2 === O(Seq("Slick", "Animal")))
      }
      "should resolve conditional values to correct alternative" in {
        val kalle = Person(
          name = Name("Kalle", "Blomkvist"),
          properties = Map("age" → "30")
        )
        assert(
          E("person" → kalle)
            .resolve("person.properties.age?'known':'unknown'") === O("known")
        )
        assert(
          E("person" → kalle)
            .resolve("person.properties.x?'known':'unknown'") === O("unknown")
        )
        assert(
          E("person" → kalle)
            .resolve("person.properties.age?:'unknown'") === O("30")
        )
        assert(
          E("person" → kalle)
            .resolve("person.properties.x?:'unknown'") === O("unknown")
        )
        assert(
          E("person" → kalle).resolve(
            "person.properties.age?person.name.first:'unknown'"
          ) === O("Kalle")
        )
        assert(
          E("person" → kalle).resolve(
            "person.properties.x?person.name.first:person.name.last"
          ) === O("Blomkvist")
        )
      }
    }

    "when consisting of XML nodes" - {
      val xml = XML.loadString("""<person id="kalle">
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
        assert(E(xml).traverse("alias", "person.alias") === Seq.empty)
      }
      "should resolve method expressions to their bound value in the nearest enclosing environment" in {
        val env = E(xml)
        val friends: Seq[Environment] = env.traverse("friend", "person.friends")
        val friendsHead = friends.head
        val persons1: Seq[Environment] =
          friendsHead.traverse("person", "friends.person")
        assert(
          persons1
            .map(_.resolve("person.name.first")) === Seq(O("Nisse"), O("Pelle"))
        )
        val persons2: Seq[Environment] =
          env.traverse("person", "person.friends.person")
        assert(
          persons2
            .map(_.resolve("person.name.first")) === Seq(O("Nisse"), O("Pelle"))
        )
        //val persons3: Seq[Environment] = env(xml)("person.friends.*")
        //assert(persons3.flatMap(_.resolve("person.name.first")) === Seq(O("Nisse"), O("Pelle"))))
      }
      "should resolve method expressions one level at a time" in {
        val env = E(xml)
        assert(env.resolve("person.name.first") === O("Kalle"))
        assert(env.resolve("person.name.last") === O("Blomkvist"))
        val persons = env.traverse("person", "person.friends.person")
        assert(persons.size === 2)
        assert(
          persons
            .map(_.resolve("person.name.first")) === Seq(O("Nisse"), O("Pelle"))
        )
      }
    }

    "when consisting of JSON nodes" - {
      val json = ujson.read("""{
                              |  "person": {
                              |    "id": "kalle",
                              |    "age": 20,
                              |    "weight": 80.5,
                              |    "colors": ["blue", "red", "green"],
                              |    "fruits": ["apple"],
                              |    "name": {
                              |      "first": "Kalle",
                              |      "last": "Blomkvist"
                              |    },
                              |    "friends": [
                              |      {
                              |        "person": {
                              |          "id": "nisse",
                              |          "alias": "Slick",
                              |          "name": {
                              |            "first": "Nisse",
                              |            "last": "Karlsson"
                              |          }
                              |        }
                              |      },
                              |      {
                              |        "person": {
                              |          "id": "pelle",
                              |          "alias": "Animal",
                              |          "name": {
                              |            "first": "Pelle",
                              |            "last": "Persson"
                              |          }
                              |        }
                              |      }
                              |    ]
                              |  }
                              |}""".stripMargin)

      "should resolve missing expressions to None" in {
        assert(E(json).traverse("alias", "person.alias") === Seq.empty)
      }
      "should resolve method expressions to their bound value in the nearest enclosing environment" in {
        val env = E(json)
        val friends: Seq[Environment] = env.traverse("friend", "person.friends")
        val friendsHead = friends.head
        val persons1: Seq[Environment] =
          friendsHead.traverse("person", "friends.person")
        val actual = persons1
          .map(R(_, "person.name.first"))
        val expected = Seq(O("Nisse"), O("Pelle"))
        assert(
          actual === expected
        )
        val persons2: Seq[Environment] =
          env.traverse("person", "person.friends.person")
        assert(
          persons2
            .map(R(_, "person.name.first")) === Seq(O("Nisse"), O("Pelle"))
        )
      }
      "should resolve method expressions one level at a time" in {
        val env = E(json)
        assert(env.resolve("person.name.first") === O("Kalle"))
        assert(env.resolve("person.name.last") === O("Blomkvist"))
        val persons = env.traverse("person", "person.friends.person")
        assert(persons.size === 2)
        assert(
          persons
            .map(_.resolve("person.name.first")) === Seq(O("Nisse"), O("Pelle"))
        )
      }
      "should resolve ints to ints and doubles to doubles" in {
        val env = E(json)
        assert(env.resolve("person.age") === O(20))
        assert(env.resolve("person.weight") === O(80.5d))
      }
      "should resolve array string items to unquoted strings" in {
        val env = E(json)
        var result = env.resolve("person.colors.0")
        assert(result === O("blue"))
        result = env.resolve("person.fruits.0")
        assert(result === O("apple"))
      }
      "should resolve negative array indices from the right" in {
        val env = E(json)
        var result = env.resolve("person.colors.-1")
        assert(result === O("green"))
        result = env.resolve("person.colors.-2")
        assert(result === O("red"))
        result = env.resolve("person.colors.-3")
        assert(result === O("blue"))
      }
    }
  }
}
