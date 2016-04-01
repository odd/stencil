package stencil

import scala.xml.NodeSeq
import stencil.Environment.Path

trait Environment {
  def lookup(name: String): Option[AnyRef]
  def resolve(exp: String): Option[AnyRef] = {
    if (isLiteral(exp)) resolveLiteral(exp)
    else if (isReplace(exp)) resolveReplace(exp)
    else lookup(exp).orElse(resolveLocal(None, Some(Path(exp))))
  }

  def apply(exp: String): Seq[Environment] = traverseValue(Path(exp).components.last, resolve(exp))
  def apply(name: String, exp: String): Seq[Environment] = traverseValue(name, resolve(exp))
  def apply(nodes: NodeSeq): XmlEnvironment = new XmlEnvironment(this, nodes)
  def apply(name: String, nodes: NodeSeq): XmlEnvironment = new XmlEnvironment(this, nodes, Some(name))
  def apply(bindings: Map[String, AnyRef]): Environment = new MapEnvironment(this, bindings)
  def apply(name: String, value: Option[AnyRef]): Environment = value match {
    case Some(v) ⇒ new MapEnvironment(this, name -> v)
    case None ⇒ new MapEnvironment(this, name -> null)
  }
  def apply(bindings: (String, AnyRef)*): Environment = new MapEnvironment(this, bindings: _*)

  private[stencil] def isLiteral(exp: String) = exp.size > 1 && exp.charAt(0) == '\'' && exp.charAt(exp.length - 1) == '\''
  private[stencil] def isReplace(exp: String) = exp.size > 1 && exp.charAt(0) == '/' && exp.charAt(exp.length - 1) == '/'
  private[stencil] def resolveLiteral(exp: String): Option[String] = Some(exp.substring(1, exp.length - 1))
  private[stencil] def resolveReplace(exp: String): Option[(String, Option[AnyRef])] = {
    val e = exp.substring(1, exp.length - 1)
    val index = e.indexOf('/')
    val pattern = e.substring(0, index)
    val replacement = e.substring(index + 1)
    Some((pattern, resolve(replacement)))
  }
  private[stencil] def flatten(o: AnyRef): Option[AnyRef] = o match {
    case null => None
    case None => None
    case xs: Traversable[AnyRef] if xs.isEmpty => None
    case Some(v: AnyRef) => flatten(v)
    case ns: NodeSeq => Some(ns)
    //case xs: Traversable[AnyRef] if (xs.size == 1) => flatten(xs.head)
    //case xs: Traversable[AnyRef] => Some(xs.flatMap(flatten))
    case x => Some(x)
  }
  private[stencil] def resolveLocal(owner: Option[AnyRef], path: Option[Path]): Option[AnyRef] = {
    (flatten(owner), path) match {
      case (None, None) ⇒ None
      case (Some(o), None) ⇒ Some(o)
      case (x, Some(p)) if x.isEmpty || x == null ⇒
        val candidates = p.candidates
        val mapped: Seq[Option[AnyRef]] = candidates.map { subPath =>
          val subPathValue = subPath.value
          val value = lookup(subPathValue)
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
                    val local = resolveLocal(Some(v), p.rest(subPath))
                    local
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
            else {
              val flattened: Option[AnyRef] = flatten(found)
              flattened
            }
        }
        val c: PartialFunction[Option[AnyRef], Option[AnyRef]] = {
          case Some(t: Traversable[AnyRef]) ⇒
            println("c ## p: " + p)
            val candidates = p.candidates.init
            println("c ## candidates: " + candidates)
            val mapped: Seq[Option[AnyRef]] = candidates.map { subPath =>
              println("c ## subPath: " + subPath)
              val value = resolveLocal(Some(t), Some(subPath))
              println("c ## value: " + value)
              value.flatMap { v ⇒
                val rest = p.rest(subPath).filterNot(!_.isSingleton)
                println("c ## rest: " + rest)
                resolveLocal(Some(v), rest)
              }
            }
            val best = mapped.collectFirst {
              case Some(x: AnyRef) => x
            }
            //best.getOrElse(None)
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
            println("Path unresolvable for owner [owner: " + owner.take(60) + ", path: " + path + "].")
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
  private[stencil] def resolveLocal2(owner: Option[AnyRef], path: Option[Path]): Option[AnyRef] = {
    (owner, path) match {
      case (None, None) ⇒ None
      case (Some(None), _) ⇒ None
      case (Some(Some(o: AnyRef)), None) ⇒ Some(o)
      case (Some(o), None) ⇒ Some(o)
      case (x, Some(p)) if x.isEmpty || x == null ⇒
        val candidates = p.candidates
        val mapped = candidates.flatMap { subPath =>
          val subPathValue = subPath.value
          val value = lookup(subPathValue)
          value.map { v ⇒
            val rest = p.rest(subPath)
            resolveLocal(Some(v), rest)
          }
        }
        val best = mapped.find(_.isDefined)
        best.getOrElse(None)
      case (Some(m: Map[String, _]), Some(p)) ⇒
        p.candidates.flatMap {
          case subPath ⇒ m.get(subPath.value).flatMap {
            case v: AnyRef ⇒ resolveLocal(Some(v), p.rest(subPath))
          }
        }.find(_ != None)
      case (Some(Some(o: AnyRef)), Some(p)) ⇒
        resolveLocal(Some(o), path)
      case (Some(nodes: NodeSeq), Some(p)) =>
        val mapped = p.candidates.map {
          case subPath ⇒
            val map = (nodes \ subPath.value).flatMap {
              case v: AnyRef ⇒
                val local = resolveLocal(Some(v), p.rest(subPath))
                local
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
        else {
          val flattened = flatten(found)
          flattened
        }
      case (Some(o: AnyRef), Some(p)) ⇒
        val result = p.components.headOption.flatMap {
          c =>
            try {
              val local = resolveLocal(Some(o.getClass.getMethod(c).invoke(o)), p.rest(c))
              local
            } catch {
              case e: NoSuchMethodException ⇒ None
            }
        }
        result
      case _ ⇒ throw new IllegalArgumentException("Unknown owner or path [owner: " + owner + ", path: " + path + "].")
    }
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

  case class Path(private val literal: String, separator: Option[Char] = None) {
    val dot = separator.getOrElse('.')
    val value = literal
    val components: Seq[String] = literal.split(dot).toSeq
    val isSingleton = components.size == 1
    def rest(literal: String): Option[Path] = rest(Path(literal))
    /*
    def rest(path: Path): Option[Path] = {
      if (this.literal == path.literal) None
      else if (value.startsWith(path.value + dot)) Some(Path(literal.substring(path.value.length + 1)))
      else None
    }
    */
    def rest(path: Path): Option[Path] = {
      if (this.literal == path.literal) None
      else if (this.components.startsWith(path.components)) {
        val rest = this.components.drop(path.components.length).mkString(dot.toString)
        println(value + ".rest(" + path.value + "): " + rest)
        Some(Path(rest))
      } else None
    }
    def candidates: Seq[Path] = {
      def inner(rest: String, list: List[Path]): List[Path] = rest.lastIndexOf(dot) match {
        case -1 ⇒ Path(rest) :: list
        case index ⇒ inner(rest.substring(0, index), Path(rest) :: list)
      }
      inner(literal, Nil).reverse
    }
  }
  object Path {
    val empty = Path("")
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
    val matchingNodes = nodes.theSeq.filter(n => this.name.exists(_ == n.label) || n.label == name)
    if (matchingNodes.isEmpty) (this.name.map(nodes \ _), nodes \ name) match {
      case (None, NodeSeq.Empty) ⇒ None
      case (Some(NodeSeq.Empty), NodeSeq.Empty) ⇒ None
      case (Some(NodeSeq.Empty), ns) ⇒ Some(ns)
      case (Some(ns), _) ⇒ Some(ns)
      case (_, ns) ⇒ Some(ns)
    } else Some(matchingNodes)
  }
  override def traverseValueMatcher = super.traverseValueMatcher.orElse {
    //case (null, Some(ns: NodeSeq)) => Seq(this)
    //case (name, Some(ns: NodeSeq)) => Seq(apply(name, nodes))
    case (null, Some(t: Traversable[AnyRef])) => Seq(this)
    case (name, Some(t: Traversable[AnyRef])) ⇒ {
      val t2 = t
      t2.map { o => apply(name, o.asInstanceOf[NodeSeq]) }.toList
    }
  }

  override def toString = name.map("\"" + _ + "\": ").getOrElse("") + nodes.toString()
}
