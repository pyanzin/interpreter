object Main extends App {
  override def main(args: Array[String]) = {
    val file = io.Source.fromFile(args(0))
    val sourceCode = try file.mkString finally file.close()

    val parser = new JParser
    val parseResult = parser.parseProgram(sourceCode)

    if (parseResult.successful) {
    	val ast = parseResult.get
      //ast.visit(fixRecursiveCall)
    	ast.eval(JPredef.getPredef)
    } else {
    	print(parseResult.toString)
    }    
  }

  def fixRecursiveCall(expr: Expr): Expr = {
    expr match {
      case Assignment(Selector(id), FuncWithClosure(args, body, closuredVars))
         if closuredVars.contains(id) => Assignment(Selector(id), FuncWithClosure(args, body, closuredVars - id))
      case _ => expr
    }
  }
}
