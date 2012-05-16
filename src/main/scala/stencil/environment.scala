package stencil

trait Environment {
  def parent: Option[Environment]
  def bind(name: String, value: AnyRef): Environment = new SubEnvironment(Some(this), name -> value)
  def bind(bindings: (String, AnyRef)*): Environment = new SubEnvironment(Some(this), bindings: _*)
  def lookupLocal(name: String): Option[AnyRef]
  def lookup(name: String): Option[AnyRef] = lookupLocal(name) match {
    case None => parent.flatMap(_.lookup(name))
    case Some(value) => Some(value)
  }
  def resolve(exp: String): AnyRef = {
    def unwrap(v: AnyRef): AnyRef = {
      if (v.isInstanceOf[Some[_]]) {
        unwrap(v.asInstanceOf[Some[_]].get.asInstanceOf[AnyRef])
      } else if (v == None) null
      else v
    }
    def call(o: AnyRef, path: String): AnyRef = {
      var result: AnyRef = null
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
            case option: Option[AnyRef] ⇒ result = unwrap(option.get)
            case _ ⇒
          }
          case e ⇒ e.printStackTrace()
        }
      }
      if (result == null) {
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
        if (index == -1) owner.get
        else call(owner.get, expression.substring(index + 1))
    } else expression
  }
  def traverse(value: AnyRef): Seq[Environment] = traverse(null, value)
  def traverse(name: String, value: AnyRef): Seq[Environment] = value match {
    case None ⇒ Seq.empty
    case null ⇒ Seq.empty
    case "" ⇒ Seq.empty
    case t: Traversable[AnyRef] ⇒
      t.map { o =>
        if (name != null) SubEnvironment(Some(this), name → o)
        else this
      }.toSeq
    case s: Option[AnyRef] =>
      s.map { o =>
        if (name != null) SubEnvironment(Some(this), name → o)
        else this
      }.toSeq
    case o: AnyRef ⇒
      if (name != null) Seq(SubEnvironment(Some(this), name → o))
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
  def lookupLocal(name: String) = map.get(name)
}
