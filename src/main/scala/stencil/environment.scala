package stencil

import com.sun.tools.example.debug.bdi.MethodNotFoundException

trait Environment {
  def parent: Option[Environment]
  def bind(name: String, value: Option[AnyRef]): Environment = value match {
    case None ⇒ new SubEnvironment(Some(this), name -> null)
    case Some(v) ⇒ new SubEnvironment(Some(this), name -> v)
  }
  def bind(bindings: (String, AnyRef)*): Environment = new SubEnvironment(Some(this), bindings: _*)
  def lookup(name: String): Option[AnyRef]
  def resolve(exp: String): Option[AnyRef] = {
     if (exp.size > 1 && exp.charAt(0) == '\'' && exp.charAt(exp.length - 1) == '\'') Some(exp.substring(1, exp.length - 1))
     else lookup(exp) match {
      case None ⇒
        resolveLocal(None, Some(Path(exp))) match {
          case None ⇒ parent.flatMap(_.resolve(exp))
          case v ⇒ v
        }
      case v ⇒ v
    }
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
    def rest(literal: String): Path = rest(Path(literal))
    def rest(path: Path): Path = {
      require(literal.startsWith(path.literal))
      Path(literal.substring(path.literal.length + 1))
    }
  }
  object Path {
    val empty = Path("")
  }
  def resolveLocal(owner: Option[AnyRef], path: Option[Path]): Option[AnyRef] = {
    (owner, path) match {
      case (None, None) ⇒ None
      case (Some(Some(o: AnyRef)), None) ⇒ Some(o)
      case (Some(o), None) ⇒ Some(o)
      case (x, Some(p)) if (x == None || x == null) ⇒
        p.candidates.collectFirst {
          case subPath ⇒ lookup(subPath.literal).map {
            v ⇒ resolveLocal(Some(v), Some(p.rest(subPath)))
          }
        }.getOrElse(None)
      case (Some(m: Map[String, _]), Some(p)) ⇒
        p.candidates.collectFirst {
          case subPath ⇒ m.get(subPath.literal).collect {
            case v: AnyRef ⇒ resolveLocal(Some(v), Some(p.rest(subPath)))
          }
        }.getOrElse(None)
      case (Some(Some(o: AnyRef)), Some(p)) ⇒
        resolveLocal(Some(o), path)
      case (Some(o: AnyRef), Some(p)) ⇒
        p.components.headOption.map { c =>
          try {
            resolveLocal(Some(o.getClass.getMethod(c).invoke(o)), Some(p.rest(c)))
          } catch {
            case e: MethodNotFoundException ⇒ None
          }
        }
      case _ ⇒ throw new IllegalArgumentException("Unknown owner or path [owner: " + owner + ", path: " + path + "].")
    }
  }
  def traverse(value: Option[AnyRef]): Seq[Environment] = traverse(null, value)
  def traverse(name: String, value: Option[AnyRef]): Seq[Environment] = value match {
    case null ⇒ Seq.empty
    case None ⇒ Seq.empty
    case Some(null) ⇒ Seq.empty
    case Some(None) ⇒ Seq.empty
    case Some("") ⇒ Seq.empty
    case Some(t: Traversable[AnyRef]) ⇒
      t.map { o =>
        if (name != null) SubEnvironment(Some(this), name → o)
        else this
      }.toSeq
    case Some(v) =>
      if (name != null) Seq(SubEnvironment(Some(this), name → v))
      else Seq(this)
  }
}
case object RootEnvironment extends Environment {
  def parent = None
  def lookup(name: String) = None
}

case class SubEnvironment(
    override val parent: Some[Environment],
    bindings: (String, AnyRef)*) extends Environment {
  val map = bindings.toMap
  def lookup(name: String) = map.get(name) match {
    case Some(null) ⇒ None
    case o ⇒ o
  }
}
