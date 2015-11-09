import scala.util.parsing.combinator._

class JParser extends JavaTokenParsers {
  def string[JString] = stringLiteral ^^
    { case x: String => JString(x) }

  def number[JNumber] = floatingPointNumber ^^
    { case x => JNumber(x.toDouble) }

  def selector[Selector] = rep1sep(ident, ".") ^^
    { case xs => Selector(xs.reverse.toList) }

  def call: Parser[Call] = simpleExpr ~ ("(" ~> repsep(expr, ",") <~ ")") ^^
    { case ex ~ args => Call(ex, args) }

  def func: Parser[Func] = ("function" ~> "(" ~> repsep(ident, ",") <~ ")" <~ "=") ~ expr ^^
    { case args ~ ex => Func(args, ex) }

  def operator: Parser[String] = "+" | "-" | "*" | "/"

  def op: Parser[Op] = simpleExpr ~ operator ~ expr ^^ 
    { case a ~ op ~ b => Op(op, a, b) }

  def expr[Expr] = func | ifElse | assign | call | block | op | number | string | selector 

  def stmts: Parser[Block] = repsep(expr, ";") <~ opt(";") ^^
    { case exprs => Block(exprs) }

  def block[Block] = "{" ~> stmts <~ "}"

  def assign: Parser[Assignment] = (selector <~ "=") ~ expr ^^
    { case s ~ ex => Assignment(s, ex)}

  def simpleExpr: Parser[Expr] = (ident ^^ { case  id => Selector(List(id)) }) | ("(" ~> expr <~ ")")

  def ifElse: Parser[IfElse] = ("if" ~> "(" ~> expr <~ ")") ~ (expr) ~ ("else" ~> expr) ^^ 
    { case cond ~ body ~ elseBody => IfElse(cond, body, elseBody) }
}
