object Main extends App {
  override def main(args: Array[String]) = {
    val file = io.Source.fromFile(args(0))
    val sourceCode = try file.mkString finally file.close()

    val parser = new JParser
    val parseResult = parser.parseProgram(sourceCode)

    if (parseResult.successful) {
    	val ast = parseResult.get
    	ast.eval(JPredef.getPredef)
    } else {
    	print(parseResult.toString)
    }    
  }
}
