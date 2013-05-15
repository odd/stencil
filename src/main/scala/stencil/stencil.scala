package stencil

import util.matching.Regex.{MatchData, Match}
import java.io.{Writer, StringWriter, StringReader, Reader}

class Stencil private (reader: Reader, tree: Stencil.Tree, val transformer: Stencil.Transformer = Stencil.defaultTransformer) {
  import Stencil._
  def this(reader: Reader) = this(reader, Stencil.parse(reader))
  def withTransformer(transformer: Transformer): Stencil = new Stencil(reader, tree, transformer.orElse(defaultTransformer))
  def apply(bindings: (String, AnyRef)*): String = {
    val environment = EmptyEnvironment.bind(bindings: _*)
    implicit val writer = new StringWriter()
    tree.contents.foreach { n ⇒ apply(n, environment) }
    writer.getBuffer.toString
  }
  private def apply(node: Node, environment: Environment)(implicit writer: Writer) {
    node match {
      case Text(data) ⇒
        write(data)
      case tag @ Tag(name, attributes, directives, contents) =>
        if (directives.isEmpty) {
          write('<')
          write(name)
          attributes.foreach {
            case (name, value) ⇒
              write(' ')
              write(name)
              write('=')
              write('"')
              write(value)
              write('"')
          }
          if (contents.isEmpty) {
            write('/')
            write('>')
          } else {
            write('>')
            contents.foreach(n ⇒ apply(n, environment))
            write('<')
            write('/')
            write(name)
            write('>')
          }
        } else {
          directives.head match {
            case d @ Namespace(prefix, uri) =>
              apply(
                tag.copy(
                  directives = directives.tail),
                environment)
            case d @ Let(transform, name, expression) =>
              apply(
                tag.copy(
                  directives = directives.tail),
                environment.bind(name, transformer(transform → environment.resolve(expression))))
            case d @ Set(transform, name, expression) =>
              environment.resolve(expression) match {
                case Some((p: String, Some(v: AnyRef))) ⇒
                  transformer(transform → Some(v)) match {
                    case Some(value) =>
                      attributes.find(_._1 == name).map(_._2.replace(p, value.toString)).foreach { replacedValue =>
                        apply(
                          tag.copy(
                            attributes = attributes.filterNot(_._1 == name) :+ ((name, replacedValue)),
                            directives = directives.tail),
                          environment)
                      }
                    case None =>
                      apply(
                        tag.copy(
                          attributes = attributes.filterNot(_._1 == name),
                          directives = directives.tail),
                        environment)
                  }
                case Some(v: AnyRef) ⇒
                  transformer(transform → Some(v)) match {
                    case Some(value) =>
                      apply(
                        tag.copy(
                          attributes = attributes.filterNot(_._1 == name) :+ ((name, value.toString)),
                          directives = directives.tail),
                        environment)
                    case None =>
                      apply(
                        tag.copy(
                          attributes = attributes.filterNot(_._1 == name),
                          directives = directives.tail),
                        environment)
                  }
                case x ⇒
                  val y = x
                  apply(
                    tag.copy(
                      attributes = attributes.filterNot(_._1 == name),
                      directives = directives.tail),
                    environment)
              }
            case d @ SetBody(transform, expression) ⇒
              environment.resolve(expression) match {
                case Some((p: String, Some(v: AnyRef))) ⇒
                  transformer(transform → Some(v)) match {
                    case Some(value) =>
                      apply(
                        tag.copy(
                          directives = directives.tail,
                          contents = tag.contents.collect {
                            case Text(data) ⇒ Text(data.replace(p, value.toString))
                            case n ⇒ n
                          }),
                        environment)
                    case None =>
                      apply(
                        tag.copy(
                          directives = directives.tail),
                        environment)
                  }
                case Some(v: AnyRef) ⇒
                  transformer(transform → Some(v)) match {
                    case Some(value) =>
                      apply(
                        tag.copy(
                          directives = directives.tail,
                          contents = Seq(Text(value.toString))),
                        environment)
                    case None =>
                      apply(
                        tag.copy(
                          directives = directives.tail),
                        environment)
                  }
                case x ⇒
                  val y = x
                  apply(
                    tag.copy(
                      directives = directives.tail),
                    environment)
              }
              /*
              apply(
                tag.copy(
                  directives = directives.tail,
                  contents = Seq(Text())),
                environment
                )*/
            case d @ Do(transform, name, expression) =>
              environment.traverseValue(name, transformer(transform → environment.resolve(expression))).foreach { env =>
                apply(
                  tag.copy(
                    directives = directives.tail),
                  env)
              }
            case d @ DoBody(transform, expression) ⇒
              environment.traverseValue(transformer(transform → environment.resolve(expression))).foreach { env =>
                apply(
                  tag.copy(
                    directives = directives.tail),
                  env)
              }
            case _ =>
          }
        }
    }
  }
  private def write(c: Char)(implicit writer: Writer) {
    writer.write(c)
    writer.flush()
  }
  private def write(s: String)(implicit writer: Writer) {
    writer.write(s)
    writer.flush()
  }
  def format(node: Node): String = node match {
    case Text(data) ⇒ data
    case Tag(name, attributes, directives, contents) ⇒
      "<" + name + attributes.map {
        case (name, value) ⇒ " " + name + "=\"" + value + "\""
      }.mkString + directives.map {
        case Let(null, name, expression) ⇒ " x:let-" + name + "=\"" + expression + "\""
        case Set(null, name, expression) ⇒ " x:set-" + name + "=\"" + expression + "\""
        case Do(null, name, expression) ⇒ " x:do-" + name + "=\"" + expression + "\""
        case Let(transform, name, expression) ⇒ " x:let--" + transform + "-" + name + "=\"" + expression + "\""
        case Set(transform, name, expression) ⇒ " x:set--" + transform + "-" + name + "=\"" + expression + "\""
        case Do(transform, name, expression) ⇒ " x:do--" + transform + "-" + name + "=\"" + expression + "\""
        case SetBody(null, expression) ⇒ " x:set" + "=\"" + expression + "\""
        case DoBody(null, expression) ⇒ " x:do" + "=\"" + expression + "\""
        case SetBody(transform, expression) ⇒ " x:set--" + transform + "=\"" + expression + "\""
        case DoBody(transform, expression) ⇒ " x:do--" + transform + "=\"" + expression + "\""
      }.mkString + (if (contents.isEmpty) "/>" else ">" + contents.map(format).mkString + "</" + name + ">")
  }
  override def toString = tree.contents.map(format).mkString
}

object Stencil {
  sealed trait Node
  sealed trait Container {
    def contents: Seq[Node]
  }
  case class Tree(contents: Seq[Node]) extends Container
  case class Text(data: String) extends Node {
    override def toString = this.productPrefix
  }
  case class Tag(name: String, attributes: Seq[(String, String)], directives: Seq[Directive], contents: Seq[Node] = Seq.empty) extends Node with Container

  type =>?[-A, +B] = PartialFunction[A, B]
  type Transformer = (Pair[String, Option[AnyRef]]) =>? Option[AnyRef]
  val defaultTransformer: Transformer = { case (_, value) ⇒ value }

  sealed trait Directive
  case class Namespace(prefix: String, uri: String) extends Directive
  case class Let(transform: String, name: String, expression: String) extends Directive
  case class Set(transform: String, name: String, expression: String) extends Directive
  case class Do(transform: String, name: String, expression: String) extends Directive
  case class SetBody(transform: String, expression: String) extends Directive
  case class DoBody(transform: String, expression: String) extends Directive

  def apply(reader: Reader): Stencil = new Stencil(reader)
  def apply(literal: String): Stencil = apply(new StringReader(literal))

  val StartOrCloseTag = """<\s*(/)?([\w:_-]+)((?:\s+(?:[\w:_-]+)\s*=\s*(?:(?:"(?:[^"]*)")|(?:'(?:[^']*)')))*)\s*(/)?>""".r
  val DirectionTag = """<(?:\s*([\w:_-]+)((?:\s+(?:[\w:_-]+)\s*=\s*(?:(?:"(?:[^"]*)")|(?:'(?:[^']*)')))*(?:\s+(?:x:[\w_-]+)\s*=\s*(?:(?:"(?:[^"]*)")|(?:'(?:[^']*)')))(?:\s+(?:[\w:_-]+)\s*=\s*(?:(?:"(?:[^"]*)")|(?:'(?:[^']*)')))*)\s*(/)?)>|(!--.*?--)>""".r
  val Attribute = """\s+(?:(x|[\w_-]+):)?([\w_-]+)\s*=\s*(?:(?:"([^"]*)")|(?:'([^']*)'))""".r

  def parse(reader: Reader): Tree = Tree(parse(asCharSequence(reader)))
  def parse(data: CharSequence): List[Node] = {
    var nodes: List[Node] = Nil
    var start = 0
    var lastMatch: Option[Match] = None
    do {
      lastMatch = DirectionTag.findFirstMatchIn(data.subSequence(start, data.length()))
      lastMatch match {
        case Some(m) =>
          val (tagStartStart, tagStartEnd) = (start + m.start, start + m.end)
          if (m.group(4) != null) {
            nodes ::= Text(data.subSequence(start, tagStartEnd).toString)
            start = tagStartEnd
          } else {
            //val tagData = data.subSequence(tagStartStart, tagStartEnd)
            nodes ::= Text(data.subSequence(start, tagStartStart).toString)
            val (attributes, directives) = parseAttributes(m.group(2))
            if (m.group(3) == null) {
              val (tagEndStart, tagEndEnd) = findBodyEndIndices(data, tagStartEnd, m.group(1))
              val bodyData = data.subSequence(tagStartEnd, tagEndStart)
              nodes ::= Tag(m.group(1), attributes, directives, parse(bodyData))
              start = tagEndEnd
            } else {
              nodes ::= Tag(m.group(1), attributes, directives)
              start = tagStartEnd
            }
          }
        case None =>
          nodes ::= Text(data.subSequence(start, data.length).toString)
      }
    } while (start < data.length && lastMatch != None)
    nodes.reverse
  }
  private def asCharSequence(reader: Reader): CharSequence = {
    val size = 2048
    val buffer = new Array[Char](size)
    var count = 0
    val builder = new StringBuilder(size)
    do {
      count = reader.read (buffer, 0, size)
      if (count > -1) builder.appendAll(buffer, 0, count)
    } while (count == size)
    builder
  }
  private def parseAttributes(data: String): (Seq[(String, String)], Seq[Directive]) = {
    var attributes: List[(String, String)] = Nil
    var directives: List[Directive] = Nil
    val matches: List[Match] = Attribute.findAllIn(data).matchData.toList
    for (m ← matches) {
      if (m.group(1) == "x") {
        directives ::= (m.group(2) match {
          case name if (name.startsWith("let--")) ⇒
            val suffix = name.substring("let--".length)
            val index = suffix.indexOf('-')
            if (index == -1) throw new IllegalStateException("Let directive with transform and empty name encountered [" + name + "=\"" + m.group(3) + "\"].")
            Let(suffix.substring(0, index), suffix.substring(index + 1), m.group(3))
          case name if (name.startsWith("set--")) ⇒
            val suffix = name.substring("set--".length)
            val index = suffix.indexOf('-')
            if (index > -1) Set(suffix.substring(0, index), suffix.substring(index + 1), m.group(3))
            else SetBody(suffix, m.group(3))
          case name if (name.startsWith("do--")) ⇒
            val suffix = name.substring("do--".length)
            val index = suffix.indexOf('-')
            if (index > -1) Do(suffix.substring(0, index), suffix.substring(index + 1), m.group(3))
            else DoBody(suffix, m.group(3))
          case name if (name.startsWith("let-")) ⇒
            Let(null, name.substring("let-".length), m.group(3))
          case name if (name.startsWith("set-")) ⇒
            Set(null, name.substring("set-".length), m.group(3))
          case name if (name.startsWith("do-")) ⇒
            Do(null, name.substring("do-".length), m.group(3))
          case name if (name == "set") ⇒ SetBody(null, m.group(3))
          case name if (name == "do") ⇒ DoBody(null, m.group(3))
          case _ ⇒ throw new IllegalStateException("Unknown directive encountered [" + m.group(0) + "]")
        })
      } else {
        attributes ::= (m.group(2), m.group(3))
      }
    }
    (attributes.reverse, directives.reverse)
  }
  private def findBodyEndIndices(data: CharSequence, offset: Int, tagName: String): (Int, Int) = {
    var start = offset
    var stack = List((tagName, null: MatchData))
    var last: Match = null
    while (stack.nonEmpty && start < data.length) {
      StartOrCloseTag.findFirstMatchIn(data.subSequence(start, data.length)) match {
        case None ⇒ throw new IllegalStateException("No matching end tag found for start tag [" + tagName + "].")
        case Some(m) ⇒
          if (m.group(1) == "/") {
            if (stack.head._1 != m.group(2)) {
              throw new IllegalStateException("No matching end tag found for start tag [" + stack.head._2 + " @ offset: " + stack.head._2.start + "].")
            }
            stack = stack.tail
          } else if (m.group(4) == null) {
            stack ::= (m.group(2), m)
          }
          last = m
      }
      start += last.end
    }
    if (last != null) (start - (last.end - last.start), start)
    else throw new IllegalStateException("No matching end tag found for start tag [" + tagName + "].")
  }
  private def findBodyEndIndices0(data: CharSequence, offset: Int, tagName: String): (Int, Int) = {
    var start = offset
    var level = 1
    var last: Match = null
    while (level > 0 && start < data.length) {
      StartOrCloseTag.findFirstMatchIn(data.subSequence(start, data.length)) match {
        case None ⇒ throw new IllegalStateException("No matching end tag found for start tag [" + tagName + "].")
        case Some(m) ⇒
          if (m.group(1) == "/")
            level -= 1
          else if (m.group(4) == null) {
            level += 1
          }
          last = m
      }
      start += last.end
    }
    if (last != null) (start - (last.end - last.start), start)
    else throw new IllegalStateException("No matching end tag found for start tag [" + tagName + "].")
  }
}

object Main {
  case class Customer(name: String)
  case class Order(date: String, customer: Customer, lines: List[OrderLine])
  case class OrderLine(product: Product, quantity: Int)
  case class Product(name: String, price: Double)

  def main(args: Array[String]) {
    val data = """<html xmlns:x="http://bitbonanza.org/stencil">
                 |  <body x:let-customer="order.customer">
                 |    <span x:set="customer.name" title="title" x:set-title="order.customer.name">ACME</span> <span x:set="order.date">2012-01-01</span>
                 |    <table x:let-orderLines="order.lines" width="100%">
                 |      <tr>
                 |        <td>Product</td>
                 |        <td>Quantity</td>
                 |      </tr>
                 |      <tr x:do-line="orderLines">
                 |        <td x:set="line.product.name">Pryttel</td>
                 |        <td x:set="line.quantity" x:set-align="right" align="left">42</td>
                 |      </tr>
                 |    </table>
                 |  </body>
                 |</html>""".stripMargin

    val stencil = Stencil(new StringReader(data))
    println(stencil)
    println(stencil("order" → Order("2012-04-11", Customer("Foo Intermedia"), OrderLine(Product("Bar", 199d), 12) :: OrderLine(Product("Baz", 79d), 3) :: Nil)))
  }
}
