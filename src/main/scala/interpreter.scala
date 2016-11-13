trait JValue extends Expr {
  def eval(context: Context) = this
}

trait Expr {
  def eval(context: Context): JValue
  
  def visit(modifier: Function[Expr, Expr]): Expr = modifier(this)

  def toBool(value: JValue): Boolean = value match {
    case JBoolean(x) => x
    case JString(x) => x.length > 0
    case JNumber(x) => x != 0
    case JArray(xs) => xs.length != 0
    case JObject(flds) => flds.size > 0
    case Undefined => false
    case Func(_, _, _) => true
    case FuncWithClosure(_, _, _) => true
  }
}

trait LeftHand extends Expr {
  def assign(context: Context, value: JValue): Unit
}

case class Func(args: List[String], body: Expr, closure: Map[String, JValue] = Map()) extends JValue

object FuncWithClosure {
  def make(args: List[String], body: Expr)(implicit symbolTable: SymbolTable): FuncWithClosure = {
    args.foreach(symbolTable.defined)
    var closureVars = symbolTable.exit
    FuncWithClosure(args, body, closureVars)
  }
}

case class FuncWithClosure(args: List[String], body: Expr, closureVars: Set[String]) extends JValue {
  override def eval(context: Context) = {
    val closured = closureVars.map(x => x -> context.get(x)).toMap
    Func(args, body, closured)
  }

  override def visit(modifier: Function[Expr, Expr]): Expr = {
    FuncWithClosure(args, modifier(body.visit(modifier)), closureVars)
  }
}

case object Undefined extends JValue {
  override def toString() = "undefined"
}

case class JBoolean(x: Boolean) extends JValue {
  override def toString() = x.toString
}

case class JObject(var fields: Map[JValue, JValue]) extends JValue {
  override def toString() = fields.map(kv => s"${kv._1} : ${kv._2}").mkString("{", ", ", "}")
}

case class JNumber(x: Double) extends JValue {
  override def toString() = if(x % 1 == 0.0) x.toInt.toString else x.toString
}

case class JString(x: String) extends JValue {
  override def toString() = x
}

case class JArray(xs: List[JValue]) extends JValue {
  override def toString() = xs.mkString("[", ", ", "]")
}

case class Context(
  val parent: Option[Context],
  var dataSet: Map[String, JValue]
) {
  def set(k: String, v: JValue) { dataSet += (k -> v) }

  def get(k: String): JValue = dataSet.getOrElse(k, parent match {
      case Some(p) => p.get(k)
      case None => Undefined
    })
}

case class Call(func: Expr, args: List[Expr]) extends Expr {
  def eval(context: Context): JValue = {
    val function = func.eval(context).asInstanceOf[Func]
    val dataset = function.args.zipAll(args.map(_.eval(context)), "", Undefined).toMap ++ function.closure
    val newContext = Context(Some(context), dataset)
    function.body.eval(newContext)
  }

  override def visit(modifier: Function[Expr, Expr]): Expr = {
    Call(modifier(func.visit(modifier)), args.map(_.visit(modifier)))
  }
}

object Selector {
  def make(id: String)(implicit symbolTable: SymbolTable) = {
    symbolTable.used(id)
    Selector(id)
  }
}

case class Selector(id: String) extends Expr with LeftHand {
  def eval(context: Context) = {
    context.get(id)
  }

  def assign(context: Context, value: JValue) = {
    context.set(id, value)
  }
}

case class Op(op: String, a: Expr, b: Expr) extends Expr {
  private val numberOps: Map[String, (Double, Double) => JValue] = Map(
    "+" -> ((a, b) => JNumber(a + b)),
    "-" -> ((a, b) => JNumber(a - b)),
    "*" -> ((a, b) => JNumber(a * b)),
    "/" -> ((a, b) => JNumber(a / b)),
    "<" -> ((a, b) => JBoolean(a < b)),
    ">" -> ((a, b) => JBoolean(a > b)),
    ">=" ->((a, b) => JBoolean(a >= b)),
    "<=" ->((a, b) => JBoolean(a <= b)),
    "%" -> ((a, b) => JNumber(a % b))
  )

  private val booleanOps: Map[String, (Boolean, Boolean) => JValue] = Map(
    "&&" -> ((a, b) => JBoolean(a && b)),
    "||" -> ((a, b) => JBoolean(a && b))    
  )

  def eval(context: Context) = (a.eval(context), b.eval(context)) match {
    case (a: JValue, b: JValue) if op == "==" => JBoolean(a == b)
    case (JBoolean(a), JBoolean(b)) => booleanOps(op)(a, b)
    case (JNumber(a), JNumber(b)) => numberOps(op)(a, b)
    case (JString(a), b: JValue) => 
      if (op == "+") JString(a + b.toString) 
        else throw new Exception("Incompatable types")
    case _ => throw new Exception("Incompatable types")
  }

  override def visit(modifier: Function[Expr, Expr]) = {
    Op(op, modifier(a.visit(modifier)), modifier(a.visit(modifier)))
  }
}

object Assignment {
  def make(left: LeftHand, right: Expr)(implicit symbolTable: SymbolTable) = {
    left match {
      case Selector(id) => symbolTable.defined(id)
    }

    Assignment(left, right)
  }
}

case class Assignment(left: LeftHand, right: Expr) extends Expr {
  def eval(context: Context) = {
    val value = right.eval(context)
    left.assign(context, value)
    value
  }
}

case class Block(stmts: List[Expr]) extends Expr {
  def eval(context: Context): JValue = {
    def eval1(stmts: List[Expr]): JValue = stmts match {
      case x :: Nil => x.eval(context)
      case x :: xs => {
        x.eval(context)
        eval1(xs)
      }
      case Nil => Undefined
    }

    eval1(stmts)
  }

  override def visit(modifier: Function[Expr, Expr]): Expr = Block(stmts.map(x => modifier(x.visit(modifier))))
}

case class IfElse(cond: Expr, body: Expr, elseBody: Expr) extends Expr {
  def eval(context: Context): JValue = {
    if (toBool(cond.eval(context)))
      body.eval(context)
    else
      elseBody.eval(context)
  }

  override def visit(modifier: Function[Expr, Expr]): Expr = 
    IfElse(modifier(cond.visit(modifier)), modifier(body.visit(modifier)), modifier(elseBody.visit(modifier)))

}

case class WhileExpr(cond: Expr, body: Expr) extends Expr {
  def eval(context: Context): JValue = {
    while (toBool(cond.eval(context))) {
      body.eval(context)
    }
    Undefined
  }

  override def visit(modifier: Function[Expr, Expr]): Expr = 
    WhileExpr(modifier(cond.visit(modifier)), modifier(body.visit(modifier)))
}

case class ArrayExpr(xs: List[Expr]) extends Expr {
  def eval(context: Context): JValue = JArray(xs.map(_.eval(context)))

  override def visit(modifier: Function[Expr, Expr]): Expr = 
    ArrayExpr(xs.map(_.visit(modifier)))
}

case class ObjectExpr(xs: Map[Expr, Expr]) extends Expr {
  def eval(context: Context): JValue = JObject(xs.map(x => (x._1.eval(context), x._2.eval(context))))

  override def visit(modifier: Function[Expr, Expr]): Expr = 
    ObjectExpr(xs.map(x => modifier(x._1.visit(modifier)) -> modifier(x._2.visit(modifier))))
}

case class Indexer(siteExpr: Expr, argExpr: Expr) extends Expr with LeftHand {
  def eval(context: Context): JValue = {
    val site = siteExpr.eval(context)
    val arg = argExpr.eval(context)
    site match {
      case s: JObject => s.fields.getOrElse(arg, Undefined)
      case s: JArray => s.xs(arg.asInstanceOf[JNumber].x.toInt)
      case _ => throw new Exception("Incompatable type")
    }
  }

  def assign(context: Context, value: JValue) {
    val site = siteExpr.eval(context)
    val arg = argExpr.eval(context)
    site match {
      case s: JObject => s.fields += (arg -> value)
      case s: JArray => ???
      case _ => throw new Exception("Incompatable type")
    }
  }
}

case class Debugger extends Expr {
  def eval(context: Context): JValue = {
    println("debugger:\n")

    val parser = new JParser

    while(true) {
      val input = readLine()
      if(input.startsWith('\032'.toString)){
        return Undefined
      } else {
        val result = parser.parseProgram(input)
        if(result.successful){
          val ast = result.get
          ast.eval(context)
        } else {
          print(result.toString)
        }        
      }      
    }
    Undefined
  }
}