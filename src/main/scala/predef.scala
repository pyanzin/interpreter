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

	def getPredef() = Context(
		None,
		Map(
			"readLine" -> readLineF,
			"writeLine" -> writeLineF
			)
	)
}