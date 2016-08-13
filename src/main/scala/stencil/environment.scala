package stencil

import scala.xml.{Node, NodeSeq}
import stencil.Environment.{Expression, Path}

trait Environment {
  def apply(exp: String): Seq[Environment] = traverseValue(Path(exp).components.last, resolve(exp))
  def lookup(name: String): Option[AnyRef]
  def resolve(exp: String): Option[AnyRef] = Expression(exp).resolve(this)
  /*
  def apply(nodes: NodeSeq): XmlEnvironment = new XmlEnvironment(this, nodes)
  def apply(name: String, nodes: NodeSeq): XmlEnvironment = new XmlEnvironment(this, nodes, Some(name))
  def apply(name: String, seq: Seq[_]): Seq[Environment] = seq match {
    case nodes: NodeSeq => Seq(new XmlEnvironment(this, nodes, Some(name)))
    case _ if seq.forall(_.isInstanceOf[Option[_]]) => seq.map(opt => traverseValue(name, opt.asInstanceOf[Option[AnyRef]))
  }
  */
  def apply(bindings: Map[String, AnyRef]): Environment = new MapEnvironment(this, bindings)
  def apply(bindings: (String, AnyRef)*): Environment = new MapEnvironment(this, bindings: _*)
  private[stencil] def apply(name: String, exp: String): Seq[Environment] = traverseValue(name, resolve(exp))
  private[stencil] def apply(name: String, value: Option[AnyRef]): Environment = value match {
    case Some(v) ⇒ new MapEnvironment(this, name -> v)
    case None ⇒ new MapEnvironment(this, name -> null)
  }

  def traverseValueMatcher: PartialFunction[(String, Option[AnyRef]), Seq[Environment]] = {
      case (_, null) ⇒ Seq.empty
      case (_, None) ⇒ Seq.empty
      case (_, Some(java.lang.Boolean.FALSE)) ⇒ Seq.empty
      case (_, Some(null)) ⇒ Seq.empty
      case (_, Some(None)) ⇒ Seq.empty
      case (_, Some("")) ⇒ Seq.empty
  }
  private[stencil] def traverseValue(value: Option[AnyRef]): Seq[Environment] = traverseValue(null, value)
  private[stencil] def traverseValue(name: String, value: Option[AnyRef]): Seq[Environment] = traverseValueMatcher((name, value))
}
object Environment {
  object Empty extends Environment {
    def lookup(name: String) = None
  }

  def apply(nodes: NodeSeq): XmlEnvironment = new XmlEnvironment(Empty, nodes)
  def apply(bindings: Map[String, AnyRef]): Environment = new MapEnvironment(Empty, bindings)
  def apply(name: String, value: Option[AnyRef]): Environment = value match {
    case Some(v) ⇒ new MapEnvironment(Empty, name -> v)
    case None ⇒ new MapEnvironment(Empty, name -> null)
  }
  def apply(bindings: (String, AnyRef)*): Environment = new MapEnvironment(Empty, bindings: _*)

  trait Expression {
    def resolve(implicit env: Environment): Option[AnyRef]
  }
  object Expression {
    val Pattern = """(^[a-zA-Z0-9_]*(?:\.[a-zA-Z0-9_]+)*$)|(^'[^']*'$)|(?:^/([^/]+)/([^/]*)/$)|(?:^(.+?)\?(.*?):(.+?)$)""".r
    def apply(exp: String): Expression = {
      val Pattern(path, literal, pattern, replacement, condition, positive, negative) = exp
      if (path != null) Path(path)
      else if (literal != null) Literal(literal)
      else if (pattern != null) Replace(pattern, replacement)
      else if (condition != null) Conditional(condition, positive, negative)
      else throw new IllegalArgumentException("Malformed expression specified: " + exp)
    }
  }
  case class Literal(str: String) extends Expression {
    override def resolve(implicit env: Environment): Option[String] = Some(str.substring(1, str.length - 1))
  }
  case class Path(private val literal: String, separator: Option[Char] = None) extends Expression {
    val dot = separator.getOrElse('.')
    val value = literal
    val components: Seq[String] = literal.split(dot).toSeq
    val isSingleton = components.size == 1

    override def resolve(implicit env: Environment): Option[AnyRef] = {
      env.lookup(literal).orElse(resolveLocal(None, Some(this)))
    }
    def rest(literal: String): Option[Path] = rest(Path(literal))
    def rest(path: Path): Option[Path] = {
      if (this.literal == path.literal) None
      else if (this.components.startsWith(path.components))
        Some(Path(this.components.drop(path.components.length).mkString(dot.toString)))
      else None
    }
    def candidates: Seq[Path] = {
      def inner(rest: String, list: List[Path]): List[Path] = rest.lastIndexOf(dot) match {
        case -1 ⇒ Path(rest) :: list
        case index ⇒ inner(rest.substring(0, index), Path(rest) :: list)
      }
      inner(literal, Nil).reverse
    }

    private def flatten(o: AnyRef): Option[AnyRef] = o match {
      case null => None
      case None => None
      case xs: Traversable[AnyRef] if xs.isEmpty => None
      case Some(v: AnyRef) => flatten(v)
      case ns: NodeSeq => Some(ns)
      //case xs: Traversable[AnyRef] if (xs.size == 1) => flatten(xs.head)
      //case xs: Traversable[AnyRef] => Some(xs.flatMap(flatten))
      case x => Some(x)
    }
    private def resolveLocal(owner: Option[AnyRef], path: Option[Path])(implicit env: Environment): Option[AnyRef] = {
      (flatten(owner), path) match {
        case (None, None) ⇒ None
        case (Some(o), None) ⇒ Some(o)
        case (x, Some(p)) if x.isEmpty || x == null ⇒
          val candidates = p.candidates
          val mapped: Seq[Option[AnyRef]] = candidates.map { subPath =>
            val subPathValue = subPath.value
            val value = env.lookup(subPathValue)
            value.flatMap { v ⇒
              val rest = p.rest(subPath)
              resolveLocal(Some(v), rest)
            }
          }
          val best: Option[AnyRef] = mapped.collectFirst {
            case Some(x: AnyRef) => x
          }
          best
        case (Some(o), Some(p)) =>
          val a: PartialFunction[Option[AnyRef], Option[AnyRef]] = {
            case Some(m: Map[String, _]) ⇒
              val results: Seq[Option[AnyRef]] = p.candidates.map {
                case subPath ⇒ m.get(subPath.value).flatMap {
                  case v: AnyRef ⇒ resolveLocal(Some(v), p.rest(subPath))
                }
              }
              results.collectFirst {
                case Some(x: AnyRef) => x
              }
          }
          val b: PartialFunction[Option[AnyRef], Option[AnyRef]] = {
            case Some(nodes: NodeSeq) =>
              val mapped = p.candidates.map {
                case subPath ⇒
                  val map = (nodes \ subPath.value).flatMap {
                    case v: AnyRef ⇒
                      resolveLocal(Some(v), p.rest(subPath))
                  }
                  flatten(map)
              }
              val found = mapped.find(_.isDefined).orElse {
                if (!p.isSingleton) None
                else {
                  val attribute: NodeSeq = nodes \ ("@" + p.value)
                  if (attribute.isEmpty) None
                  else Some(attribute.text)
                }
              }
              if (found.isEmpty) None
              else flatten(found)
            case Some(seq: Seq[Node]) if seq.forall(_.isInstanceOf[Node]) =>
              val nodes = NodeSeq.fromSeq(seq)
              val mapped = p.candidates.map {
                case subPath ⇒
                  val map = (nodes \ subPath.value).flatMap {
                    case v: AnyRef ⇒
                      resolveLocal(Some(v), p.rest(subPath))
                  }
                  flatten(map)
              }
              val found = mapped.find(_.isDefined).orElse {
                if (!p.isSingleton) None
                else {
                  val attribute: NodeSeq = nodes \ ("@" + p.value)
                  if (attribute.isEmpty) None
                  else Some(attribute.text)
                }
              }
              if (found.isEmpty) None
              else flatten(found)
          }
          val c: PartialFunction[Option[AnyRef], Option[AnyRef]] = {
            case Some(t: Traversable[AnyRef]) ⇒
              val candidates = p.candidates.init.filterNot(_ == p)
              val mapped: Seq[Option[AnyRef]] = candidates.map { subPath =>
                val value = resolveLocal(Some(t), Some(subPath))
                value.flatMap { v ⇒
                  val rest = p.rest(subPath).filterNot(!_.isSingleton)
                  resolveLocal(Some(v), rest)
                }
              }
              val best = mapped.collectFirst {
                case Some(x: AnyRef) => x
              }
              best
          }
          val d: PartialFunction[Option[AnyRef], Option[AnyRef]] = {
            case Some(o) ⇒
              val result: Option[AnyRef] = p.components.headOption.flatMap {
                c =>
                  try {
                    val method = o.getClass.getMethod(c)
                    val result = method.invoke(o)
                    val local = resolveLocal(Some(result), p.rest(c))
                    local
                  } catch {
                    case e: NoSuchMethodException ⇒ None
                  }
              }
              result
          }
          val e: PartialFunction[Option[AnyRef], Option[AnyRef]] = {
            case _ ⇒
              // new IllegalArgumentException("### Unknown owner or path [owner: " + owner + ", path: " + path + "].").printStackTrace()
              None
          }
          def firstDefinedResult(fs: PartialFunction[Option[AnyRef], Option[AnyRef]]*)(opt: Option[AnyRef]): Option[AnyRef] = {
            (for {
              f <- fs if f.isDefinedAt(opt)
              result = f(opt)
            } yield result).collectFirst {
              case Some(x: AnyRef) => x
            }
          }
          firstDefinedResult(a, b, c, d, e)(Some(o))
      }
    }
  }
  object Path {
    val empty = Path("")
  }
  case class Replace(pattern: String, replacement: String) extends Expression {
    override def resolve(implicit env: Environment): Option[(String, Option[AnyRef])] = {
      Some((pattern, Expression(replacement).resolve))
    }
  }
  case class Conditional(condition: String, positive: String, negative: String) extends Expression {
    override def resolve(implicit env: Environment): Option[AnyRef] = {
      val conditionValue: Option[AnyRef] = Expression(condition).resolve
      val flattened = conditionValue match {
        case null ⇒ None
        case None ⇒ None
        case Some(java.lang.Boolean.FALSE) ⇒ None
        case Some(null) ⇒ None
        case Some(None) ⇒ None
        case Some("") ⇒ None
        case x => x
      }
      flattened.flatMap { o =>
        if (positive.length > 0) Expression(positive).resolve
        else conditionValue
      }.orElse(Expression(negative).resolve)
    }
  }
}
abstract class SubEnvironment(val parent: Environment) extends Environment {
  override def resolve(exp: String) = super.resolve(exp).orElse(parent.resolve(exp))
}
case class MapEnvironment(override val parent: Environment, map: Map[String, AnyRef]) extends SubEnvironment(parent) {
  def this(parent: Environment, bindings: (String, AnyRef)*) = this(parent, bindings.toMap)

  def lookup(name: String) = map.get(name) match {
    case Some(null) ⇒ None
    case Some(t: Traversable[AnyRef]) if t.isEmpty ⇒ None
    case o ⇒ o
  }

  override def traverseValueMatcher = super.traverseValueMatcher.orElse {
    case (null, Some(t: Traversable[AnyRef])) => Seq(this)
    case (name, Some(t: Traversable[AnyRef])) ⇒ t.map { o => apply(name → o) }.toSeq
    case (null, Some(v)) => Seq(this)
    case (name, Some(v)) => Seq(apply(name → v))
  }
}

case class XmlEnvironment(override val parent: Environment, nodes: NodeSeq, name: Option[String] = None) extends SubEnvironment(parent) {
  def lookup(name: String) = {
    //val matchingNodes1 = nodes.theSeq.filter(n => this.name.contains(n.label) || n.label == name)
    val matchingNodes1 = nodes.theSeq.filter(n => n.label == name)
    //val matchingNodes = nodes.filter(n => this.name.contains(n.label) || n.label == name)
    val matchingNodes = nodes.filter(n => n.label == name)
    if (matchingNodes.isEmpty) (this.name.map(nodes \ _), nodes \ name) match {
      case (None, NodeSeq.Empty) ⇒ None
      case (Some(NodeSeq.Empty), NodeSeq.Empty) ⇒ None
      case (Some(NodeSeq.Empty), ns) ⇒ Some(ns)
      case (Some(ns), _) ⇒ Some(ns)
      case (_, ns) ⇒ Some(ns)
    } else Some(matchingNodes)
  }
  override def traverseValueMatcher: PartialFunction[(String, Option[AnyRef]), Seq[Environment]] = super.traverseValueMatcher.orElse {
    case (null, Some(t: Traversable[AnyRef])) => Seq(this)
    case (name, Some(t: Traversable[AnyRef])) ⇒ t.map { o => apply(name → o) }.toSeq
    case (null, Some(v)) => Seq(this)
    case (name, Some(v)) => Seq(apply(name → v))
    /*
    //case (null, Some(ns: NodeSeq)) => Seq(this)
    //case (name, Some(ns: NodeSeq)) => Seq(apply(name, nodes))
    case (null, Some(t: Traversable[AnyRef])) => Seq(this)
    //case (field, Some(t: Traversable[AnyRef])) if t.isEmpty || flatten(t).forall(_.isInstanceOf[NodeSeq]) ⇒
    case (field, Some(t: Traversable[AnyRef])) ⇒
      apply(field, t)
      val t2: Traversable[AnyRef] = t
      t2.map { o => apply(field, o) }.toList
    case _ => throw new IllegalStateException("!")
    */
  }

  override def toString = name.map("\"" + _ + "\": ").getOrElse("") + nodes.toString()
}
