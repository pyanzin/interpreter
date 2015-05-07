import scala.util.parsing.combinator._

class JParser extends JavaTokenParsers {
  def string[JString] = stringLiteral ^^
    { case x: String => JString(x) }

  def number[JNumber] = floatingPointNumber ^^
    { case x => JNumber(x.toDouble) }

  def selector[Selector] = repsep(ident, ".") ^^
    { case xs => Selector(xs.reverse.toList) }

  def call[Call] = expr ~ ("(" ~> repsep(expr, ",") <~ ")") ^^
    { case expr ~ args => Call(expr, args) }

  def func[Func] = ("function" ~> "(" ~> repsep(ident, ",") <~ ")" <~ "=") ~ expr ^^
    { case args ~ expr => Func(args, expr) }

  def operator[String] = "+" | "-" | "*" | "/"

  def op[Op] = expr ~ operator ~ expr ^^ 
    { case a ~ op ~ b => Op(op, a, b) }

  def expr[Expr] = number | string | selector 
}
