object JPredef {
	val readLineF = Func(List(), new Expr {
		def eval(context: Context) = JString(readLine())
	})

	val writeLineF = Func(List("x"), new Expr {
		def eval(context: Context) = {
			print(context.dataSet("x") + "\n")
			Undefined
		}
	})

	val len = Func(List("xs"), new Expr {
		def eval(context: Context) = {
			JNumber(context.dataSet("xs").asInstanceOf[JArray].xs.length)		
		}
	})

	def getPredef() = Context(
		None,
		Map("readLine" -> readLineF,
			"writeLine" -> writeLineF,
			"len" -> len
		)
	)
}