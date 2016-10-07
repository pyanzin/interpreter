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
    def eval(context: Context) = context.dataSet("xs") match {
      case xs: JArray => JNumber(xs.xs.length)
      case xs: JString => JNumber(xs.toString.length)
    }
  })

  val head = Func(List("xs"), new Expr {
    def eval(context: Context) = {
      context.dataSet("xs").asInstanceOf[JArray].xs.head 
    }
  })

  val tail = Func(List("xs"), new Expr {
    def eval(context: Context) = {
      JArray(context.dataSet("xs").asInstanceOf[JArray].xs.tail)
    }
  })

  val cons = Func(List("x", "xs"), new Expr {
    def eval(context: Context) = {
      JArray(context.dataSet("x") :: context.dataSet("xs").asInstanceOf[JArray].xs)
    }
  })

  val str = Func(List("x"), new Expr {
    def eval(context: Context) = {
      JString(context.dataSet("x").toString)
    }
  })

  val number = Func(List("x"), new Expr {
    def eval(context: Context) = {
      JNumber(context.dataSet("x").toString.toDouble)
    }
  })

  def getPredef() = Context(
    None,
    Map("readLine" -> readLineF,
      "writeLine" -> writeLineF,
      "len" -> len,
      "head" -> head,
      "tail" -> tail,
      "cons" -> cons,
      "str" -> str,
      "number" -> number,
      "undefined" -> Undefined
    )
  )
}