/*
package stencil

import scala.xml.NodeSeq

trait Environment {
  def parent: Option[Environment]
  def bind(nodes: NodeSeq): Environment = nodes match {
    case NodeSeq.Empty => this
    case _ => new XmlEnvironment(Some(this), nodes)
  }
  def bind(name: String, value: Option[AnyRef]): Environment = value match {
    case None ⇒ new SubEnvironment(Some(this), name -> null)
    case Some(v) ⇒ new SubEnvironment(Some(this), name -> v)
  }
  def bind(bindings: (String, AnyRef)*): Environment = new SubEnvironment(Some(this), bindings: _*)
  def lookup(name: String): Option[AnyRef]
  def resolve(exp: String): Option[AnyRef] = {
     if (exp.size > 1 && exp.charAt(0) == '\'' && exp.charAt(exp.length - 1) == '\'') resolveLiteral(exp)
     else if (exp.size > 1 && exp.charAt(0) == '/' && exp.charAt(exp.length - 1) == '/') resolveReplace(exp)
     else lookup(exp) match {
      case None ⇒
        resolveLocal(None, Some(Path(exp))) match {
          case None ⇒ parent.flatMap(_.resolve(exp))
          case v ⇒ v
        }
      case v ⇒ v
    }
  }
  def resolveLiteral(exp: String): Option[String] = Some(exp.substring(1, exp.length - 1))
  def resolveReplace(exp: String): Option[(String, Option[AnyRef])] = {
    val e = exp.substring(1, exp.length - 1)
    val index = e.indexOf('/')
    val pattern = e.substring(0, index)
    val replacement = e.substring(index + 1)
    Some((pattern, resolve(replacement)))
  }

  case class Path(literal: String) {
    def components: Seq[String] = literal.split('.').toSeq
    def candidates: Seq[Path] = {
      def inner(rest: String, list: List[Path]): List[Path] = rest.lastIndexOf('.') match {
        case -1 ⇒ Path(rest) :: list
        case index ⇒ inner(rest.substring(0, index), Path(rest) :: list)
      }
      inner(literal, Nil).reverse
    }
    def rest(literal: String): Option[Path] = rest(Path(literal))
    def rest(path: Path): Option[Path] = {
      if ((literal.startsWith(path.literal + "."))) Some(Path(literal.substring(path.literal.length + 1)))
      else None
    }
  }
  object Path {
    val empty = Path("")
  }
  def resolveLocal(owner: Option[AnyRef], path: Option[Path]): Option[AnyRef] = {
    (owner, path) match {
      case (None, None) ⇒ None
      case (Some(None), _) ⇒ None
      case (Some(Some(o: AnyRef)), None) ⇒ Some(o)
      case (Some(o), None) ⇒ Some(o)
      case (x, Some(p)) if (x == None || x == null) ⇒
        val candidates = p.candidates
        val mapped = candidates.flatMap { subPath =>
          val subLiteral = subPath.literal
          val lookuped = lookup(subLiteral)
          lookuped.map { v ⇒
            val rest = p.rest(subPath)
            resolveLocal(Some(v), rest)
          }
        }
        val best = mapped.find(_ != None)
        best.getOrElse(None)
      case (Some(m: Map[String, _]), Some(p)) ⇒
        p.candidates.flatMap {
          case subPath ⇒ m.get(subPath.literal).flatMap {
            case v: AnyRef ⇒ resolveLocal(Some(v), p.rest(subPath))
          }
        }.find(_ != None)
      case (Some(Some(o: AnyRef)), Some(p)) ⇒
        resolveLocal(Some(o), path)
      case (Some(nodes: NodeSeq), Some(p)) =>
        def flatten(o: AnyRef): Option[AnyRef] = o match {
          case null => None
          case None => None
          case xs: Traversable[AnyRef] if (xs.isEmpty) => None
          case Some(v: AnyRef) => flatten(v)
          case ns: NodeSeq => Some(ns)
          case xs: Traversable[AnyRef] if (xs.size == 1) => {
            flatten(xs.head)
          }
          case xs: Traversable[AnyRef] => {
            val map = xs.flatMap(flatten)
            Some(map)
          }
          case x => Some(x)
        }
        val mapped = p.candidates.map {
          case subPath ⇒
            val sp = subPath
            val ns = nodes \ subPath.literal
            val map = ns.flatMap {
              case v: AnyRef ⇒
                val local = resolveLocal(Some(v), p.rest(subPath))
                local
            }
            flatten(map)
        }
        val found = mapped.find(_.nonEmpty)
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
  def traverse(exp: String): Seq[Environment] = traverseValue(resolve(exp))
  def traverse(name: String, exp: String): Seq[Environment] = traverseValue(name, resolve(exp))
  def traverseSub(name: String, t: Traversable[AnyRef]): Seq[Environment]
  def traverseSub(name: String, o: AnyRef): Seq[Environment]
  private[stencil] def traverseValue(value: Option[AnyRef]): Seq[Environment] = traverseValue(null, value)
  private[stencil] def traverseValue(name: String, value: Option[AnyRef]): Seq[Environment] = value match {
    case null ⇒ Seq.empty
    case None ⇒ Seq.empty
    case Some(java.lang.Boolean.FALSE) ⇒ Seq.empty
    case Some(null) ⇒ Seq.empty
    case Some(None) ⇒ Seq.empty
    case Some("") ⇒ Seq.empty
    case Some(t: Traversable[AnyRef]) ⇒
      traverseSub(name, t)
      /*
    case Some(t: Traversable[AnyRef]) if false ⇒
      t.map { o =>
        if (name != null) SubEnvironment(Some(this), name → o)
        else this
      }.toSeq
      */
    case Some(v) =>
      traverseSub(name, v)
      /*
    case Some(v) if false =>
      if (name != null) Seq(SubEnvironment(Some(this), name → v))
      else Seq(this)
      */
  }
}
case object RootEnvironment extends Environment {
  def parent = None
  def lookup(name: String) = None

  def traverseSub(name: String, t: Traversable[AnyRef]) = Seq.empty

  def traverseSub(name: String, o: AnyRef) = Seq.empty
}

case class SubEnvironment(
    override val parent: Some[Environment],
    bindings: (String, AnyRef)*) extends Environment {
  val map = bindings.toMap
  def lookup(name: String) = map.get(name) match {
    case Some(null) ⇒ None
    case o ⇒ o
  }

  //def sub(bindings: (String, AnyRef)*): Environment = SubEnvironment(Some(this), bindings: _*)
  def traverseSub(name: String, t: Traversable[AnyRef]) = {
    t.map { o =>
      if (name != null) SubEnvironment(Some(this), name → o)
      else this
    }.toSeq
  }

  def traverseSub(name: String, v: AnyRef) = {
    if (name != null) Seq(SubEnvironment(Some(this), name → v))
    else Seq(this)
  }
}

case class XmlEnvironment(
    override val parent: Some[Environment],
    nodes: NodeSeq, name: Option[String] = None) extends Environment {
  def lookup(name: String) = nodes(n => this.name.exists(_ == n.label) || n.label == name) match {
    case NodeSeq.Empty ⇒ (this.name.map(nodes \ _), nodes \ name) match {
      case (None, NodeSeq.Empty) ⇒ None
      case (Some(NodeSeq.Empty), NodeSeq.Empty) ⇒ None
      case (Some(NodeSeq.Empty), ns) ⇒ Some(ns)
      case (Some(ns), _) ⇒ Some(ns)
      case (_, ns) ⇒ Some(ns)
    }
    case ns ⇒ Some(ns)
  }

  def traverseSub(name: String, t: Traversable[AnyRef]) = {
    t.map { o =>
      if (name != null) XmlEnvironment(Some(this), o.asInstanceOf[NodeSeq], Some(name))
      else this
    }.toSeq
  }

  def traverseSub(name: String, o: AnyRef) = {
    if (name != null) Seq(XmlEnvironment(Some(this), o.asInstanceOf[NodeSeq], Some(name)))
    else Seq(this)
  }


  /*
  override def resolveLocal(owner: Option[AnyRef], path: Option[Path]): Option[AnyRef] = super.resolveLocal(owner, path).orElse((owner, path) match {
    case (Some(nodes: NodeSeq), Some(p)) =>
      val mapped = p.candidates.flatMap {
        case subPath ⇒
          val sp = subPath
          val ns = nodes \ subPath.literal
          val map = ns.flatMap {
            case v: AnyRef ⇒
              val local = resolveLocal(Some(v), p.rest(subPath))
              local
          }
          map
      }
      var found = mapped.collect {
        case Some(v) => v
        case s: Seq[_] => s
      }
      if (found.isEmpty) None
      else Some(found)
    case (o, p) => None
  })
  */
}
 */
