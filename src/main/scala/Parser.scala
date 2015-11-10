import scala.util.parsing.combinator._

class JParser extends JavaTokenParsers {
  def parseProgram(s: String) = parseAll(stmts, s)

  def string[JString] = stringLiteral ^^
    { case x: String => JString(x.stripPrefix("\"").stripSuffix("\"")) }

  def number[JNumber] = floatingPointNumber ^^
    { case x => JNumber(x.toDouble) }

  def bool: Parser[JBoolean] = ("true" | "false") ^^ {
      case "true" => JBoolean(true) 
      case _ => JBoolean(false)
  }

  def objectExpr: Parser[ObjectExpr] = "{" ~> repsep(objectMember, ",") <~ "}" ^^ 
    { case exprs => ObjectExpr(exprs.toMap) }

  def objectMember: Parser[(Expr, Expr)] = (expr) ~ (":" ~> expr) ^^
    { case k ~ v => (k, v) }

  def selector[Selector] = rep1sep(ident, ".") ^^
    { case xs => Selector(xs.reverse.toList) }

  def call: Parser[Call] = selector ~ ("(" ~> repsep(expr, ",") <~ ")") ^^
    { case ex ~ args => Call(ex, args) }

  def func: Parser[Func] = ("function" ~> "(" ~> repsep(ident, ",") <~ ")" <~ "=") ~ expr ^^
    { case args ~ ex => Func(args, ex) }

  def operator: Parser[String] = "+" | "-" | "*" | "/" | ">" | "<" | "<=" | ">="

  def op: Parser[Op] = simpleExpr ~ operator ~ expr ^^ 
    { case a ~ op ~ b => Op(op, a, b) }

  def expr[Expr] = func | ifElse | assign | call | block | arrayExpr | objectExpr | op | number | bool | string | selector 

  def stmts: Parser[Block] = repsep(expr, ";") <~ opt(";") ^^
    { case exprs => Block(exprs) }

  def block[Block] = "{" ~> stmts <~ "}"

  def assign: Parser[Assignment] = (selector <~ "=") ~ expr ^^
    { case s ~ ex => Assignment(s, ex)}

  def simpleExpr: Parser[Expr] = (ident ^^ { case id => Selector(List(id)) }) | ("(" ~> expr <~ ")") | call | string | bool | number

  def ifElse: Parser[IfElse] = ("if" ~> "(" ~> expr <~ ")") ~ (expr) ~ ("else" ~> expr) ^^ 
    { case cond ~ body ~ elseBody => IfElse(cond, body, elseBody) }

  def arrayExpr: Parser[ArrayExpr] = "[" ~> repsep(expr, ",") <~ "]" ^^
    { case arr => ArrayExpr(arr) }
}
  