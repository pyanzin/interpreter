trait JValue extends Expr {
  def eval(context: Context) = this
}

trait Expr {
  def eval(context: Context): JValue
}

case class Func(args: List[String], body: Expr) extends JValue

case object Undefined extends JValue

case class JObject(var fields: Map[String, JValue]) extends JValue

case class JNumber(x: Double) extends JValue

case class JString(x: String) extends JValue

case class Context(
  val parent: Option[Context],
  var dataSet: Map[String, JValue]
)

case class Call(func: Expr, args: List[Expr]) extends Expr {
  def eval(context: Context): JValue = {
    val function = func.eval(context).asInstanceOf[Func]
  	val dataset = function.args.zipAll(args.map(_.eval(context)), "", Undefined).toMap
  	val newContext = Context(Some(context), dataset)
    function.body.eval(newContext)
  }
}

case class Selector(ids: List[String]) extends Expr {
  def eval(context: Context) = {
    context.dataSet(ids.head)
  }

  def assign(context: Context, value: JValue) = {
    context.dataSet = context.dataSet ++ Map(ids.head -> value)
  }
}

case class Op(op: String, a: Expr, b: Expr) extends Expr {
  private val ops: Map[String, (Double, Double) => Double] = Map(
    "+" -> (_+_),
    "-" -> (_-_),
    "*" -> (_*_),
    "/" -> (_/_)
  )
  def eval(context: Context) = (a.eval(context), b.eval(context)) match {
  	case (JNumber(a), JNumber(b)) => JNumber(ops(op)(a, b))
  	case (JString(a), JString(b)) => 
  	  if (op == "+") JString(a + b) 
  	  	else throw new Exception("Incompatable types")
  	case _ => throw new Exception("Incompatable types")
  }
}

case class Assignment(left: Selector, right: Expr) extends Expr {
  def eval(context: Context) = {
    val value = right.eval(context)
    left.assign(context, value)
    value
  }
}