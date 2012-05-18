package stencil

trait Environment {
  def parent: Option[Environment]
  def bind(name: String, value: Option[AnyRef]): Environment = value match {
    case None ⇒ new SubEnvironment(Some(this), name -> null)
    case Some(v) ⇒ new SubEnvironment(Some(this), name -> v)
  }
  def bind(bindings: (String, AnyRef)*): Environment = new SubEnvironment(Some(this), bindings: _*)
  def lookupLocal(name: String): Option[AnyRef]
  def lookup(name: String): Option[AnyRef] = {
    if (name.size > 1 && name.charAt(0) == '\'' && name.charAt(name.length - 1) == '\'') Some(name.substring(1, name.length - 1))
    else lookupLocal(name) match {
      case None => parent.flatMap(_.lookup(name))
      case Some(value) => Some(value)
    }
  }
  def resolve(exp: String): Option[AnyRef] = {
    def unwrap(v: AnyRef): Option[AnyRef] = {
      if (v.isInstanceOf[Some[_]]) unwrap(v.asInstanceOf[Some[_]].get.asInstanceOf[AnyRef])
      else if (v == None) None
      else None
    }
    def call(o: AnyRef, path: String): Option[AnyRef] = {
      var result: Option[AnyRef] = None
      val index = path.indexOf('.')
      val owner = unwrap(o)
      if (owner != null) {
        try {
          val method = owner.getClass.getMethod(path)
          val wrappedResult = method.invoke(owner)
          result = unwrap(wrappedResult)
        } catch {
          case e: NoSuchMethodException ⇒ owner match {
            case map: Map[String, AnyRef] ⇒ result = unwrap(map(path))
            case some: Some[AnyRef] ⇒ result = unwrap(some.get)
            case _ ⇒
          }
          case e ⇒ e.printStackTrace()
        }
      }
      if (result == None) {
        if (index == -1) {
          println("### Expression unresolvable [owner: " + owner + ", path: " + path + "].")
        } else {
          val subOwner = unwrap(owner.getClass.getMethod(path.substring(0, index)).invoke(owner))
          if (subOwner != null) {
            result = unwrap(call(subOwner, path.substring(index + 1)))
          }
        }
      }
      result
    }
    /*
    val startIndex = math.max(exp.indexOf('\''), 0)
    val endIndex = math.max(exp.lastIndexOf('\''), exp.length)
    val prefix = exp.substring(0, startIndex)
    val suffix = exp.substring(endIndex)
    val expression = exp.substring(startIndex, endIndex)
    var owner = lookup(expression)
    var index = expression.lastIndexOf('.')
    while (owner == None && index > -1) {
      owner = lookup(expression.substring(0, index))
      if (owner == None) index = expression.substring(0, index).lastIndexOf('.')
    }
    if (owner.nonEmpty) {
        if (index == -1) prefix + owner.get + suffix
        else prefix + call(owner.get, expression.substring(index + 1)) + suffix
    } else prefix + expression + suffix
    */
    val expression = exp
    var owner = lookup(expression)
    var index = expression.lastIndexOf('.')
    while (owner == None && index > -1) {
      owner = lookup(expression.substring(0, index))
      if (owner == None) index = expression.substring(0, index).lastIndexOf('.')
    }
    if (owner.nonEmpty) {
        if (index == -1) owner
        else call(owner.get, expression.substring(index + 1))
    } else None
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
  def lookupLocal(name: String) = None
}

case class SubEnvironment(
    override val parent: Some[Environment],
    bindings: (String, AnyRef)*) extends Environment {
  val map = bindings.toMap
  def lookupLocal(name: String) = map.get(name) match {
    case Some(null) ⇒ None
    case o ⇒ o
  }
}
