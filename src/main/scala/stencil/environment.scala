package stencil

import scala.collection.immutable.ListMap
import scala.language.postfixOps
import scala.util.Try
import scala.util.control.NonFatal
import scala.xml.{Elem, NodeSeq}

case class Environment(parent: Environment, instance: Any)(
    implicit accessor: Environment.Accessor
) {
  import Environment._
  def bind(name: String, value: Any): Environment =
    Environment(this, Map(name -> value))
  def bind(bindings: (String, Any)*): Environment =
    Environment(this, bindings.toMap)
  def bind(bindings: Map[String, Any]): Environment =
    Environment(this, bindings)
  private def bind(value: Any): Environment = Environment(this, value)
  def traverse(name: String, exp: String): Seq[Environment] =
    traverse(name, Expression(exp))
  def traverse(name: String, exp: Expression): Seq[Environment] = exp match {
    case Literal(Empty())    => Seq()
    case Literal(value)      => Seq(bind(name, value))
    case Path(_, Empty(), _) => Seq()
    case p @ Path(_, _, _) =>
      val resolved: Option[Any] = resolve(p)
      val mapped: Option[Seq[Environment]] = resolved.map {
        case t: Traversable[_] =>
          t.map { v =>
            bind(name, v)
          }.toSeq
        case Some(v) =>
          bind(name, v).?.toSeq
        case v =>
          bind(name, v).?.toSeq
      }
      mapped.getOrElse(Seq.empty)
    case Conditional(_, condition, positive, negative) =>
      val resolvedCondition: Option[Any] = resolve(condition)
      val result: Option[Any] = resolvedCondition.flatMap {
        case Empty()                  => resolve(negative)
        case c if positive.length > 0 => resolve(positive)
        case c                        => c ??
      }
      result.map(bind(name, _)).toSeq
  }
  def resolve(exp: String): Option[Any] = resolve(Expression(exp))
  def resolve(exp: Expression): Option[Any] = {
    val value: Option[Any] = resolveLocal(exp)
    //rintln(s"resolve: exp = $exp, value = $value")
    val value2 =
      if (value.isEmpty && parent != null)
        parent.?.map(_.resolve(exp)).getOrElse(None)
      else value
    //rintln(s"resolve: exp = $exp, value2 = $value2")
    val value3 = value2.map(_.??).collect {
      case Some(x) => x
    }
    //rintln(s"resolve: exp = $exp, value3 = $value3")
    value3
  }
  private[stencil] def resolveLocal(exp: Expression): Option[Any] = {
    val handled: Traversable[(String, Option[Any])] = handlers.collect {
      case (name, handler) if handler.isDefinedAt((exp, instance)) =>
        //rintln(s"resolveLocal: exp = $exp, name = $name, instance = $instance")
        (name, handler.apply((exp, instance)))
    }
    //rintln(s"resolveLocal: exp = $exp, handled = ${handled.toList}")
    val mapped: Option[Any] = handled.collectFirst {
      case (k, v @ NonEmpty()) =>
        //rintln(s"resolveLocal: exp = $exp, k = $k, v = $v")
        v
    }.??
    //rintln(s"resolveLocal: exp = $exp, mapped = $mapped")
    mapped
  }
  private[this] def access(v: Any, acc: String): Option[Any] = {
    val vo = v.??
    vo.map { v =>
      if (accessor.f.isDefinedAt((v, acc))) accessor.f.apply((v, acc)).??
      else v
    }
  }
  private[this] type Handler = (Expression, Any) =>? Option[Any]
  private[this] val handlers: Map[String, Handler] =
    ListMap(
      "empty expression" -> {
        case (Literal(Empty()), _) => None
        case (Literal(value), _) =>
          Some(value)
        case (Conditional(_, condition, positive, negative), _) =>
          val resolvedCondition = resolve(condition)
          if (resolvedCondition.isEmpty) resolve(negative)
          else if (positive.length > 0) resolve(positive)
          else resolvedCondition
        case (Path(_, Empty(), _), _) => None
      },
      "empty instance" -> {
        case (_, Empty()) => None
      },
      "mapped" -> {
        case (p @ Path(_, value, acc), map: Map[_, _])
            if map.forall(t => t._1.isInstanceOf[String]) && map
              .asInstanceOf[Map[String, Any]]
              .contains(value) =>
          val result = map.asInstanceOf[Map[String, Any]].get(value)
          //rintln(s"mapped: p = $p, value = $value, map = $map, result = $result")
          access(result, acc)
      },
      "binding" -> {
        case (p @ Path(_, segment, acc), (name, value)) if segment == name =>
          access(value, acc)
      },
      "xml" -> {
        case (p @ Path(_, name, acc), elem: Elem)
            if p.isSingleton && elem.label == name =>
          access(elem, acc)
        case (p @ Path(_, name, acc), nodes: NodeSeq) if p.isSingleton =>
          def select(ns: NodeSeq, child: String): Seq[Any] = {
            def extract(ns: NodeSeq): List[Any] = {
              val elems = (ns \ child).toList
              if (elems.nonEmpty) elems
              else {
                val attrs = (ns \ ("@" + child)).toList.map(_.text)
                attrs
              }
            }
            if (ns.isEmpty) Seq.empty
            else if (ns.size == 1) {
              if (ns.isInstanceOf[Elem]) extract(ns)
              else extract(ns.head)
            } else {
              ns.toList.flatMap { n =>
                if (n.isInstanceOf[Elem]) extract(n)
                else extract(n.head)
              }
            }
          }
          val selected: Seq[Any] = select(nodes, name)
          if (selected.isEmpty && name == "text") nodes.text.?
          else if (selected.isEmpty) None
          else if (selected.size == 1) access(selected.head, acc)
          else access(selected, acc)
      },
      "traversable" -> {
        case (p @ Path(_, index, acc), t: Traversable[_])
            if Try(index.toInt).isSuccess =>
          val seq = t.toSeq
          val v = p.toString.toInt
          val i =
            math.max(0, math.min(seq.size - 1, if (v < 0) seq.size + v else v))
          val result = seq(i).??
          access(result, acc)
        case (p @ Path(_, _, acc), t: Traversable[_])
            if !t.isInstanceOf[NodeSeq] =>
          //case (p@Path(_, _, acc), t: Traversable[_]) =>
          val options = t.map { o =>
            Environment(parent, o).resolveLocal(p)
          }
          val values = options.collect {
            case Some(v) => v
          }
          if (values.isEmpty) None
          else if (values.size == 1) access(values.head, acc)
          else access(values, acc)
      },
      "method" -> {
        case (p @ Path(_, name, acc), o: AnyRef) if p.isSingleton =>
          try {
            val value: Option[Any] = o.getClass.getMethod(name).invoke(o).??
            access(value, acc)
          } catch {
            case e: NoSuchMethodException ⇒ None
          }
      },
      "complex" -> {
        case (p @ Path(_, _, acc), _) if p.isComplex =>
          val candidates: Seq[(Expression, Expression)] = p.candidates
          //val candidatesList: Seq[(Expression, Expression)] = candidates.toList
          //rintln(s"complex: p = $p, candidates = $candidatesList")
          val mapped: Seq[Option[Any]] = candidates.map {
            case (candidate, rest) =>
              val local1: Option[Any] = resolveLocal(candidate)
              //rintln(s"complex: p = $p, local1 = $local1, candidate = $candidate, rest = $rest")
              val local3 = local1.flatMap { value =>
                val local2: Option[Any] = bind(value).resolveLocal(rest)
                //rintln(s"complex: p = $p, local2 = $local2, rest = $rest, value = $value")
                local2
              }
              //rintln(s"complex: p = $p, local3 = $local3, candidate = $candidate, rest = $rest")
              local3
          }
          val result = mapped.collectFirst {
            case Some(v) ⇒ v
          }
          //rintln(s"complex: p = $p, candidates = $candidatesList, mapped = ${mapped.toList}, result = $result")
          access(result, acc)
      }
    )
}
object Environment {
  def apply(): Environment = Environment(null, null)(Accessor.default)
  def apply(value: Any)(implicit accessor: Accessor): Environment =
    Environment(null, value)

  case class Accessor(f: (Any, String) =>? Any) extends AnyVal {
    def apply(o: (Any, String)) = f(o)
  }
  object Accessor {
    implicit val default: Accessor = Accessor({
      case (o, Empty()) => o
      case (o, "class") => o.getClass.getName
      case (o, "kind")  => o.getClass.getSimpleName.toLowerCase()
    })
  }
  sealed trait Expression {
    def literal: String
    override def toString: String = literal
  }
  object Expression {
    //val Pattern = """(^[a-zA-Z0-9_:-]*(?:\.[a-zA-Z0-9_:-]+)*$)|(^'[^']*'$)|(?:^/([^/]+)/([^/]*)/$)|(?:^(.+?)\?(.*?):(.+?)$)""".r
    val Pattern =
      """(?:(^[a-zA-Z0-9_:-]*(?:[\[\.][a-zA-Z0-9_:-]+\]?)*)(?:@([a-zA-Z0-9_:-]+))?$)|(?:^'([^']*)'$)|(?:^(.+?)\?(.*?):(.+?)$)""".r
    def apply(exp: String): Expression = {
      if (exp.empty) Literal("")
      else
        try {
          val Pattern(path, acc, literal, condition, positive, negative) = exp
          if (path != null)
            Path(exp, path.replace('[', '.').replace(']', '.'), acc)
          else if (literal != null) Literal(literal)
          //else if (pattern != null) Replace(pattern, replacement)
          else if (condition != null)
            Conditional(exp, condition, positive, negative)
          else
            throw new IllegalArgumentException(
              "Malformed expression specified: " + exp
            )
        } catch {
          case NonFatal(_) => Literal(exp)
        }
    }
  }
  case class Literal(str: String) extends Expression {
    override def literal: String = str
  }
  case class Path(literal: String, value: String, accessor: String)
      extends Expression {
    val segments: List[String] = value.split('.').toList
    val isEmpty: Boolean = segments.isEmpty
    val isSingleton: Boolean = segments.size == 1
    val isComplex: Boolean = segments.size > 1
    val candidates: Seq[(Expression, Expression)] = {
      val xs: Seq[List[String]] = segments.inits.toSeq.tail.init
      xs.map { segments =>
        val candidate = Expression(segments.mkString("."))
        val rest = Expression(value.drop(candidate.literal.length + 1))
        (candidate, rest)
      }
    }
  }
  /*
  case class Replace(pattern: String, replacement: String) extends Expression {
    override def evaluate(implicit env: Environment): Option[String] = {
      Expression(replacement).evaluate.map(v => String.valueOf(v).replace(pattern, ))
    }
  }*/
  case class Conditional(literal: String,
                         condition: String,
                         positive: String,
                         negative: String)
      extends Expression
}

/*
class Environment(private parent: Environment, instance: Any) {
  import Environment._

  def bind(bindings: (String, Any)*): Environment = {
    if (bindings.isEmpty) this // TODO: Is this correct or should it instead shadow equally named parental mapping (if any) with null?
    else new Environment(this, bindings.toMap)
  }
  def bind(name: String, value: Any): Environment = bind(name -> value)

  def get(name: String): Option[Any] = (parent, instance) match {
    case (_, Empty) => None
    case (_, map: Map[String, Any]) => map.get(name)??
    case (null, elem: Elem) => elem.??.filter(_ => name == elem.label)
    case (parent, elem: Elem) => (elem \ name).??.orElse((elem \@ name).??)
    case (null, t: Traversable[_]) => ???
    case (parent)
    val o: Option[Any] = bindings.get(name).??
    o.orElse(parent.get(name))
  }

  def resolve(exp: String): Option[Any] = resolve(Expression(exp))
  def resolve(exp: Expression): Option[Any] = exp match {
    case Literal(str) => Some(str.substring(1, str.length - 1))
    case Path(fields) => fields match {
      case Nil => None
      case field :: Nil => get(field)
      case xs =>
        def find(fields: List[String]): Option[(String, List[String], Option[Any])] = {
          fields.inits.toSeq.init.zipWithIndex.map {
            case (fs, i) =>
              val e = fs.mkString(".")
              (e, fields.drop(fields.size - i), get(e))
          }.find(_._3.isDefined)
        }
        def flatten(o: Any): Option[Any] = o match {
          case null => None
          case None => None
          case xs: Traversable[_] if xs.isEmpty => None
          case Some(v) => flatten(v)
          case ns: NodeSeq => ns??
          case xs: Traversable[_] if xs.size == 1 => flatten(xs.head)
          case xs: Traversable[_] => xs.flatMap(flatten)??
          case x => x??
        }

        flatten(find(xs).flatMap {
          case (e, Nil, value) => value ??
          case (e, rest, None) => None
          case (e, rest, Some(value)) => bind(e, value).resolve(rest.mkString("."))
        })
    }
    case Conditional(condition, positive, negative) =>
      val conditionValue: Option[Any] = Expression(condition).evaluate(this)
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
        if (positive.length > 0) Expression(positive).evaluate(this)
        else conditionValue
      }.orElse(Expression(negative).evaluate(this))
  }

  def select(exp: String): Seq[Environment] = select(null, Expression(exp))
  def select(name: String, exp: String): Seq[Environment] = select(name, Expression(exp))
  def select(name: String, exp: Expression): Seq[Environment] = exp match {
    case Literal(str) => Seq(bind(name, str))
    case Path(fields) => fields match {
      case Nil => Seq.empty
      case xs =>
        def find(fields: List[String]): Option[(String, List[String], Option[Any])] = {
          fields.inits.toSeq.init.zipWithIndex.map {
            case (fs, i) =>
              val e = fs.mkString(".")
              (e, fields.drop(fields.size - i), get(e))
          }.find(_._3.isDefined)
        }
        find(xs).map {
          case (e, _, None) => Seq(this)
          case (e, Nil, Some(value)) => Seq(bind(e, value))
          case (e, rest, Some(value)) => bind(e, value).select(rest.mkString("."))
        }.getOrElse(Seq.empty)
    }
  }
}
object Environment {
  def apply(value: Any): Environment = apply(Map("." -> value))
  def apply(nodes: NodeSeq): Environment = apply(Map("." -> nodes))
  def apply(bindings: (String, Any)*): Environment = apply(bindings.toMap)
  def apply(bindings: Map[String, Any]): Environment = new Environment(null, bindings)

  case class XmlElement(parent: Environment, name: String, element: Elem) extends Environment {
    override def get: Option[Any] = element ??
    override def get(name: String): Option[Any] = {
      if (name.contains('.') || name.contains('[')) parent.get(name)
      else this.name.??.filter(_ == name).map(_ => nodes).orElse {
        (nodes match {
          case elem: Elem if elem.label == name => nodes ??
          case attr: Attribute if attr.key == name => attr.text.??
          case node: Elem => (node \ name).??.orElse((node \ ("@" + name)).text.??)
          case _ => nodes.map(node => (node \ name).??.orElse((node \ ("@" + name)).text.??)) ??
        }).orElse(parent.get(name))
      }
    }
  }
  case class Indexed(parent: Environment, name: String, seq: Seq[Any]) extends Environment {
    override def get: Option[Any] = seq ??
    override def get(name: String): Option[Any] = {
      try {
        val v = name.toInt
        val i = math.max(0, math.min(seq.size - 1, if (v < 0) seq.size + v else v))
        seq(i)??
      } catch {
        case e: NumberFormatException =>
          seq.map(o => bind(null, o).resolve(name)).collect {
            case Some(x) => x
          }??
      }
    }.orElse(parent.get(name))
  }
  case class Referenced(parent: Environment, name: String, value: Any) extends Environment {
    override def get: Option[Any] = value ??
    override def get(name: String): Option[Any] = {
      value.??.filter(_ => name == this.name).orElse {
        try {
          value.getClass.getMethod(name).invoke(value) ??
        } catch {
          case e: NoSuchMethodException ⇒ None
        }
      }.orElse(parent.get(name))
    }
  }
}

/*
case class Environment2(parent: Environment, instance: AnyRef, name: Option[String] = None) {
  def this(parent: Environment, bindings: (String, AnyRef)*) = this(parent, bindings.toMap)

  def apply(nodes: NodeSeq): Environment = new Environment(this, nodes)
  def apply(bindings: Map[String, AnyRef]): Environment = new Environment(this, bindings)
  def apply(bindings: (String, AnyRef)*): Environment = new Environment(this, bindings: _*)
  def apply(exp: String): Seq[Environment] = apply(Path(exp).components.last, select(exp))

  private[stencil] def apply(name: String, exp: String): Seq[Environment] = apply(name, select(exp))
  private[stencil] def apply(name: String, value: Option[AnyRef]): Seq[Environment] = value match {
    case Some(v) ⇒ Seq(new Environment(this, name -> v))
    case None ⇒ Seq.empty //Seq(new Environment(this, name -> null))
  }
  private[stencil] def apply(value: Option[AnyRef]): Seq[Environment] = apply(null, value)
  //private[stencil] def apply(name: String, value: Option[AnyRef]): Seq[Environment] = traverseValueMatcher((name, value))

  def select(exp: String): Option[AnyRef] = select(Expression(exp))
  def select(exp: Expression): Option[AnyRef] = exp.select(this).orElse(parent.select(exp))

  def format(exp: String)(implicit formatter: Formatter): String = formatter(select(exp))
  def format(exp: Expression)(implicit formatter: Formatter): String = formatter(select(exp))
  def get(name: String): Option[AnyRef] = instance match {
    case map: Map[AnyRef, AnyRef] =>
      map.get(name) match {
        case None => None
        case Some(null) ⇒ None
        case Some(t: Traversable[AnyRef]) if t.isEmpty ⇒ None
        case Some(x) ⇒ Environment.flatten(x)
      }
    case nodes: NodeSeq =>
      val matchingNodes1 = nodes.theSeq.filter(n => n.label == name)
      //val matchingNodes = nodes.filter(n => this.name.contains(n.label) || n.label == name)
      val matchingNodes = nodes.filter(n => n.label == name)
      if (matchingNodes.isEmpty) (this.name.map(nodes \ _), nodes \ name, nodes \ ("@" + name)) match {
        case (None, NodeSeq.Empty, NodeSeq.Empty) ⇒ None
        case (Some(NodeSeq.Empty), NodeSeq.Empty, NodeSeq.Empty) ⇒ None
        case (Some(NodeSeq.Empty), ns, _) ⇒ Some(ns)
        case (Some(ns), _, _) ⇒ Some(ns)
        case (_, _, Attribute(_, value, _)) ⇒ Some(value)
        case (_, ns, _) ⇒ Some(ns)
      } else Some(matchingNodes)
    case x ⇒ Environment.flatten(x)
  }
  /*
  private[stencil] def traverseValueMatcher: PartialFunction[(String, Option[AnyRef]), Seq[Environment]] = {
    case (_, null) ⇒ Seq.empty
    case (_, None) ⇒ Seq.empty
    case (_, Some(java.lang.Boolean.FALSE)) ⇒ Seq.empty
    case (_, Some(null)) ⇒ Seq.empty
    case (_, Some(None)) ⇒ Seq.empty
    case (_, Some("")) ⇒ Seq.empty
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
 */
  override def toString = name.map("\"" + _ + "\": ").getOrElse("") + instance.toString
}
object Environment2 {
  val Empty: Environment = new Environment(null) {
    override def get(name: String): Option[AnyRef] = None
    override def select(exp: Expression): Option[AnyRef] = exp.select(this)
  }

  def apply(bindings: Map[String, AnyRef]): Environment = Empty(bindings)
  def apply(name: String, value: Option[AnyRef]): Seq[Environment] = Empty(name, value)
  def apply(bindings: (String, AnyRef)*): Environment = Empty(bindings: _*)
  def apply(nodes: NodeSeq): Environment = Empty(nodes)

  trait Expression {
    def select(implicit env: Environment): Option[AnyRef]
  }
  object Expression {
    val Pattern = """(^[a-zA-Z0-9_:-]*(?:\.[a-zA-Z0-9_:-]+)*$)|(^'[^']*'$)|(?:^/([^/]+)/([^/]*)/$)|(?:^(.+?)\?(.*?):(.+?)$)""".r
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
    override def select(implicit env: Environment): Option[String] = Some(str.substring(1, str.length - 1))
  }
  case class Path(private val literal: String, separator: Option[Char] = None) extends Expression {
    val dot = separator.getOrElse('.')
    val value = literal
    val components: Seq[String] = literal.split(dot).toSeq
    val isSingleton = components.size == 1

    override def select(implicit env: Environment): Option[AnyRef] = {
      env.get(literal).orElse(selectLocal(None, Some(this)))
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

    private def selectLocal(owner: Option[AnyRef], path: Option[Path])(implicit env: Environment): Option[AnyRef] = {
      (flatten(owner), path) match {
        case (None, None) ⇒ None
        case (Some(o), None) ⇒ Some(o)
        case (x, Some(p)) if x.isEmpty || x == null ⇒
          val candidates = p.candidates
          val mapped: Seq[Option[AnyRef]] = candidates.map { subPath =>
            val subPathValue = subPath.value
            val value = env.get(subPathValue)
            value.flatMap { v ⇒
              val rest = p.rest(subPath)
              selectLocal(Some(v), rest)
            }
          }
          val best: Option[AnyRef] = mapped.collectFirst {
            case Some(x: AnyRef) => x
          }
          best
        case (Some(o), Some(p)) =>
          val a: PartialFunction[Option[AnyRef], Option[AnyRef]] = {
            case Some(m: Map[String, _]) ⇒
              val results: Seq[Option[AnyRef]] = p.candidates.map { subPath ⇒
                m.get(subPath.value).flatMap {
                  case v: AnyRef ⇒ selectLocal(Some(v), p.rest(subPath))
                }
              }
              results.collectFirst {
                case Some(x: AnyRef) => x
              }
          }
          val b: PartialFunction[Option[AnyRef], Option[AnyRef]] = {
            case Some(nodes: NodeSeq) =>
              val mapped = p.candidates.map { subPath ⇒
                val map = (nodes \ subPath.value).flatMap { v: AnyRef =>
                  selectLocal(Option(v), p.rest(subPath))
                }
                flatten(map)
              }
              val found = mapped.find(_.isDefined).orElse {
                if (!p.isSingleton) None
                else {
                  val attribute: NodeSeq = nodes \ ("@" + p.value)
                  if (attribute.isEmpty) None
                  else Some(attribute)
                }
              }
              if (found.isEmpty) None
              else flatten(found)
            case Some(seq: Seq[_]) if seq.forall(_.isInstanceOf[Node]) =>
              val nodes = NodeSeq.fromSeq(seq.asInstanceOf[Seq[Node]])
              val mapped = p.candidates.map {
                case subPath if nodes.isInstanceOf[Group] ⇒
                  val map = null
                  flatten(map)
                case subPath ⇒
                  val map = (nodes \ subPath.value).flatMap { v: AnyRef ⇒
                    selectLocal(Some(v), p.rest(subPath))
                  }
                  flatten(map)
              }
              val found = mapped.find(_.isDefined).orElse {
                if (!p.isSingleton) None
                else {
                  val attribute: NodeSeq = nodes \ ("@" + p.value)
                  if (attribute.isEmpty) None
                  else Some(attribute)
                }
              }
              if (found.isEmpty) None
              else flatten(found)
          }
          val c: PartialFunction[Option[AnyRef], Option[AnyRef]] = {
            case Some(t: Traversable[AnyRef]) ⇒
              val candidates = p.candidates.filterNot(_ == p) //p.candidates.init.filterNot(_ == p)
              val mapped: Seq[Option[AnyRef]] = candidates.map { subPath =>
                val value = selectLocal(Some(t), Some(subPath))
                value.flatMap { v ⇒
                  val rest = p.rest(subPath).filterNot(!_.isSingleton)
                  selectLocal(Some(v), rest)
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
                    val local = selectLocal(Some(result), p.rest(c))
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
    override def select(implicit env: Environment): Option[(String, Option[AnyRef])] = {
      Some((pattern, Expression(replacement).select))
    }
  }
  case class Conditional(condition: String, positive: String, negative: String) extends Expression {
    override def select(implicit env: Environment): Option[AnyRef] = {
      val conditionValue: Option[AnyRef] = Expression(condition).select
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
        if (positive.length > 0) Expression(positive).select
        else conditionValue
      }.orElse(Expression(negative).select)
    }
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

}
 */
 */
