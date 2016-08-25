import scala.language.postfixOps
import scala.xml.NodeSeq
package object stencil {
  type =>?[-A, +B] = PartialFunction[A, B]
  type Formatter = Any =>? String
  object Formatter {
    implicit val Default: Formatter = {
      case s: String => s
      case null => ""
      case None => ""
      case Some(v) => Default(v)
      case ns: NodeSeq => ns.toString()
      case seq: Seq[_] => seq.map(Default).mkString(", ")
      case x => x.toString
    }
  }
  implicit class RichTypedAny[T](val t: T) extends AnyVal {
    def `?`: Option[T] = t match {
      case Empty() => None
      case _ => Some(t)
    }
  }
  implicit class RichAny(val o: Any) extends AnyVal {
    def empty: Boolean = o match {
      case 0 | 0L | 0F | 0D | false => true
      case () => true
      case null => true
      case None => true
      case s: String if s.trim.isEmpty => true
      case t: Traversable[_] if t.isEmpty => true
      case x => false
    }
    def nonEmpty: Boolean = !o.empty
    def `??`: Option[Any] = o match {
      case Empty() => None
      case Some(v) => v`??`
      case x => Some(x)
    }
  }
  object Empty {
    def unapply(o: Any): Boolean = o.empty
  }
  object NonEmpty {
    def unapply(o: Any): Boolean = o.empty
  }
}