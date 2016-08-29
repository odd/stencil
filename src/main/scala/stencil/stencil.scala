package stencil

import util.matching.Regex.{Match, MatchData}
import java.io._

import stencil.Environment.Expression

import scala.xml.NodeSeq

class Stencil private (private[stencil] val tree: Stencil.Tree)(implicit factory: StencilFactory) {
  import Stencil._
  def this(reader: Reader)(implicit factory: StencilFactory) = this(Stencil.parse(reader))
  def apply(bindings: (String, AnyRef)*)(implicit formatter: Formatter): String = apply(Environment(bindings.toMap))
  def apply(nodes: NodeSeq)(implicit formatter: Formatter): String = apply(Environment(nodes))
  def apply(environment: Environment)(implicit formatter: Formatter): String = {
    implicit val writer = new StringWriter()
    tree.contents.foreach { n ⇒ writeNodeWith(n, environment) }
    writer.getBuffer.toString
  }
  def format(node: Node): String = node match {
    case Text(data) ⇒ data
    case Tag(name, attributes, directives, contents) ⇒
      "<" + name + attributes.map {
        case (name, value) ⇒ " " + name + "=\"" + value + "\""
      }.mkString + directives.map {
        case Namespace(prefix, uri) => " xmlns:x" + prefix + "=\"" + uri + "\""
        case Let(name, expression) ⇒ " x:let-" + name + "=\"" + expression + "\""
        case Set(name, expression) ⇒ " x:set-" + name + "=\"" + expression + "\""
        case Do(name, expression) ⇒ " x:do-" + name + "=\"" + expression + "\""
        case SetBody(expression) ⇒ " x:set" + "=\"" + expression + "\""
        case DoBody(expression) ⇒ " x:do" + "=\"" + expression + "\""
        case Include(path) ⇒ " x:include" + "=\"" + path + "\""
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
  case class Tag(name: String, attributes: Seq[(String, String)], directives: Seq[Directive[_]], contents: Seq[Node] = Seq.empty) extends Node with Container

  /*
  type Transformer = (String, Option[AnyRef]) =>? Option[AnyRef]
  val defaultTransformer: Transformer = {
    case (_, Some(ns: NodeSeq)) ⇒ Option(ns.text)
    case (_, value) ⇒ value
  }
  */

  case class Expression(data: Seq[Expression.Data]) {
    import Expression._
    def format(implicit formatter: Formatter, env: Environment): String = data.foldLeft("") {
      case (s, Literal(value)) =>
        s + value
      case (s, Path(literal)) =>
        s + (env.resolve(literal) match {
          case Empty() => ""
          case Some(t: Traversable[_]) ⇒
            t.foldLeft("") {
              case (s, v) => s + formatter(v)
            }
          case Some(v) ⇒
            formatter(v)
        })
    }
  }
  object Expression {
    sealed trait Data
    case class Literal(value: String) extends Data
    case class Path(literal: String) extends Data

    val Pattern = """(?:(?:#(\w+))|(?:#\{([^}]+)\}))""".r
    def parse(literal: String): Expression = {
      var data: List[Data] = Nil
      var rest = literal
      var mo: Option[Match] = Pattern.findFirstMatchIn(rest)
      while (mo.isDefined) {
        mo.foreach { m =>
          val n = if (m.group(1) != null) 1 else 2
          data = Path(m.group(n)) :: Literal(m.before.toString) :: data
          rest = rest.substring(m.end)
          mo = Pattern.findFirstMatchIn(rest)
        }
      }
      if (data.isEmpty) Expression(List(Path(rest)))
      else Expression((Literal(rest) :: data).reverse)
    }
  }
  sealed trait Directive[T] {
    def value: T
    def format(implicit formatter: Formatter, env: Environment): String = value.toString
  }
  case class Namespace(prefix: String, value: String) extends Directive[String]
  case class Let(name: String, value: String) extends Directive[String]
  case class Do(name: String, value: String) extends Directive[String]
  case class DoBody(value: String) extends Directive[String]
  case class Set(name: String, value: Expression) extends Directive[Expression]
  case class SetBody(value: Expression) extends Directive[Expression]
  case class Include(value: Expression) extends Directive[Expression]

  def apply(literal: String): Stencil = fromString(literal)
  def apply(reader: Reader): Stencil = fromReader(reader)

  def fromString(literal: String)(implicit factory: StencilFactory = MapStencilFactory): Stencil = apply(new StringReader(literal))
  def fromReader(reader: Reader)(implicit factory: StencilFactory = MapStencilFactory): Stencil = new Stencil(reader)

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
    } while (start < data.length && lastMatch.isDefined)
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
  private def parseAttributes(data: String): (Seq[(String, String)], Seq[Directive[_]]) = {
    var attributes: List[(String, String)] = Nil
    var directives: List[Directive[_]] = Nil
    val matches: List[Match] = Attribute.findAllIn(data).matchData.toList
    for (m ← matches) {
      if (m.group(1) == "x") {
        directives ::= (m.group(2) match {
          /*
          case name if name.startsWith("let--") ⇒
            val suffix = name.substring("let--".length)
            val index = suffix.indexOf('-')
            if (index == -1) throw new IllegalStateException("Let directive with transform and empty name encountered [" + name + "=\"" + m.group(3) + "\"].")
            Let(suffix.substring(0, index), suffix.substring(index + 1), m.group(3))
          case name if name.startsWith("set--") ⇒
            val suffix = name.substring("set--".length)
            val index = suffix.indexOf('-')
            if (index > -1) Set(suffix.substring(0, index), suffix.substring(index + 1), m.group(3))
            else SetBody(suffix, m.group(3))
          case name if name.startsWith("do--") ⇒
            val suffix = name.substring("do--".length)
            val index = suffix.indexOf('-')
            if (index > -1) Do(suffix.substring(0, index), suffix.substring(index + 1), m.group(3))
            else DoBody(suffix, m.group(3))
            */
          case name if name.startsWith("let-") ⇒ Let(name.substring("let-".length), m.group(3))
          case name if name.startsWith("do-") ⇒ Do(name.substring("do-".length), m.group(3))
          case name if name == "do" ⇒ DoBody(m.group(3))
          case name if name.startsWith("set-") ⇒ Set(name.substring("set-".length), Expression.parse(m.group(3)))
          case name if name == "set" ⇒ SetBody(Expression.parse(m.group(3)))
          case name if name == "include" => Include(Expression.parse(m.group(3)))
          case _ ⇒ throw new IllegalStateException("Unknown directive encountered [" + m.group(0) + "]")
        })
      } else {
        attributes ::= m.group(2) -> m.group(3)
      }
    }
    (attributes.reverse, directives.reverse)
  }
  private def findBodyEndIndices(data: CharSequence, offset: Int, tagName: String): (Int, Int) = {
    var start = offset
    var stack = List((tagName, start))
    var last: Match = null
    while (stack.nonEmpty && start < data.length) {
      val subSequence: CharSequence = data.subSequence(start, data.length)
      StartOrCloseTag.findFirstMatchIn(subSequence) match {
        case None ⇒ throw new IllegalStateException("No matching end tag found for start tag [" + tagName + "].")
        case Some(m) ⇒
          if (m.group(1) == "/") {
            if (stack.head._1 != m.group(2)) {
              throw new IllegalStateException("No matching end tag found for start tag [" + stack.head._1 + " @ offset: " + stack.head._2 + "].")
            }
            stack = stack.tail
          } else if (m.group(4) == null) {
            stack ::= m.group(2) -> start
          }
          last = m
      }
      start += last.end
    }
    if (last != null) (start - (last.end - last.start), start)
    else throw new IllegalStateException("No matching end tag found for start tag [" + tagName + "].")
  }
  private def writeNodeWith(node: Node, env: Environment)(implicit factory: StencilFactory, formatter: Formatter, writer: Writer): Unit = {
    writeNode(node)(env, factory, formatter, writer)
  }
  private def writeNode(node: Node)(implicit env: Environment, factory: StencilFactory, formatter: Formatter, writer: Writer): Unit = {
    /*
    def formatValue: Any =>? String = {
      case ns: NodeSeq => ns.text
      case Some(x) => formatValue.apply(x)
      case t: Traversable[_] => t.map(formatValue).mkString(", ")
      case x => String.valueOf(x)
    }
    */
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
              write(formatter(value))
              write('"')
          }
          if (contents.isEmpty) {
            write('/')
            write('>')
          } else {
            write('>')
            contents.foreach(n ⇒ writeNode(n))
            write('<')
            write('/')
            write(name)
            write('>')
          }
        } else {
          directives.head match {
            case d @ Namespace(prefix, _) =>
              writeNode(tag.copy(directives = directives.tail))
            case d @ Let(name, value) =>
              env.traverse(name, value).foreach { e =>
                writeNodeWith(tag.copy(directives = directives.tail), e)
              }
            case d @ Do(name, value) =>
              env.traverse(name, value).foreach { e =>
                writeNodeWith(tag.copy(directives = directives.tail), e)
              }
            case d @ DoBody(value) ⇒
              env.traverse(name, value).foreach { e =>
                writeNodeWith(tag.copy(directives = directives.tail), e)
              }
            case d @ Set(name, expression) =>
              val result = expression.format
              if (result.isEmpty)
                writeNode(tag.copy(
                  attributes = attributes.filterNot(_._1 == name),
                  directives = directives.tail))
              else
                writeNode(tag.copy(
                  attributes = attributes.filterNot(_._1 == name) :+ ((name, result)),
                  directives = directives.tail))
            case d @ SetBody(expression) ⇒
              val result = expression.format
              if (result.isEmpty)
                writeNode(tag.copy(
                  directives = directives.tail))
              else
                writeNode(tag.copy(
                  directives = directives.tail,
                  contents = Seq(Text(formatter(result)))))
            case d @ Include(expression) ⇒
              val result = expression.format
              val inclusion = factory.produce(result)
              inclusion.tree.contents.foreach { node =>
                writeNode(node)
              }
            case _ =>
          }
        }
    }
  }
  private def write(c: Char)(implicit writer: Writer): Unit = {
    writer.write(c.toInt)
    writer.flush()
  }
  private def write(s: String)(implicit writer: Writer): Unit = {
    writer.write(s)
    writer.flush()
  }
}

trait StencilFactory {
  def produce(path: String): Stencil
}
object MapStencilFactory extends StencilFactory {
  import collection.mutable

  val map = mutable.Map[String, Stencil]()

  override def produce(path: String): Stencil = {
    map.getOrElse(path, throw new IllegalArgumentException("No stencil found for path: " + path))
  }

  def produce(path: String, literal: String): Stencil = {
    map.getOrElseUpdate(path, Stencil(literal))
  }
}

case class FileSystemStencilFactory(root: File) extends StencilFactory {
  import collection.mutable

  val cache = mutable.Map[File, (Stencil, Long)]()

  override def produce(path: String): Stencil = produce(new File(root, path))
  def produce(file: File): Stencil = {
    require(file.exists(), "File not found: " + file.getPath)
    fetch(file)
  }

  private def fetch(file: File): Stencil = {
    val (stencil, timestamp) = cache.getOrElseUpdate(file, load(file) -> System.currentTimeMillis())
    if (file.lastModified() <= timestamp) stencil
    else {
      val stencil = load(file)
      cache.put(file, stencil -> System.currentTimeMillis())
      stencil
    }
  }

  private def load(file: File): Stencil = {
    require(file.exists(), "File not found: " + file.getPath)
    val reader = new FileReader(file)
    try Stencil.fromReader(reader)(this) finally reader.close()
  }
}

object Main {
  case class Customer(name: String)
  case class Order(date: String, customer: Customer, lines: List[OrderLine])
  case class OrderLine(product: Product, quantity: Int)
  case class Product(name: String, price: Double)

  def main(args: Array[String]): Unit = {
    import Formatter.default

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
