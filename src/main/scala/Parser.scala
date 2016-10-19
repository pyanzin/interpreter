import scala.util.parsing.combinator._

class JParser extends JavaTokenParsers {
  implicit val symbolTable = new SymbolTable(Scope(Set(), Set()))

  def parseProgram(s: String) = parseAll(stmts, s)

  def string[JString] = stringLiteral ^^
    { case x: String => JString(x.stripPrefix("\"").stripSuffix("\"")) }

  def number[JNumber] = floatingPointNumber ^^
    { case x => JNumber(x.toDouble) }

  def bool: Parser[JBoolean] = ("true" | "false") ^^ {
      case "true" => JBoolean(true) 
      case _ => JBoolean(false)
  }

  def identifier = ident

  def objectExpr: Parser[ObjectExpr] = "{" ~> repsep(objectMember, ",") <~ "}" ^^ 
    { case exprs => ObjectExpr(exprs.toMap) }

  def objectMember: Parser[(Expr, Expr)] = (expr) ~ (":" ~> expr) ^^
    { case k ~ v => (k, v) }

  def selector[Selector] = ident ^^
    { case x => Selector(x) }

  def call: Parser[Call] = selector ~ ("(" ~> repsep(expr, ",") <~ ")") ^^
    { case ex ~ args => Call(ex, args) }

  def func: Parser[FuncWithClosure] = {
    symbolTable.enter
    ("(" ~> repsep(ident, ",") <~ ")" <~ "=>") ~ expr ^^
      { case args ~ ex => FuncWithClosure(args, ex) }
  }

  def operator: Parser[String] = "%" | "+" | "-" | "*" | "/" | "<=" | ">=" | ">" | "<" | "==" | "&&" | "||"

  def op: Parser[Op] = simpleExpr ~ operator ~ expr ^^ 
    { case a ~ op ~ b => Op(op, a, b) }

  def expr[Expr] = debugger | func | ifElse | whileExpr | assign | call | block | arrayExpr | objectExpr | op | number | bool | string | selector | simpleExpr

  def stmts: Parser[Block] = repsep(expr, ";") <~ opt(";") ^^
    { case exprs => Block(exprs) }

  def block[Block] = "{" ~> stmts <~ "}"

  def leftHand: Parser[LeftHand] = selector // | indexer

  def indexer: Parser[Indexer] = expr ~ ("[" ~> expr <~ "]") ^^
    { case site ~ arg => Indexer(site, arg) }

  def assign: Parser[Assignment] = (leftHand <~ "=") ~ expr ^^
    { case left ~ ex => Assignment(left, ex)}

  def simpleExpr: Parser[Expr] = (ident ^^ { case id => Selector(id) }) | ("(" ~> expr <~ ")") | call | string | bool | number

  def ifElse: Parser[IfElse] = ("if" ~> "(" ~> expr <~ ")") ~ (expr) ~ opt("else" ~> expr) ^^ 
    { case cond ~ body ~ elseBody => IfElse(cond, body, elseBody.getOrElse(Undefined)) }

  def whileExpr: Parser[WhileExpr] = ("while" ~> "(" ~> expr <~ ")") ~ expr ^^ 
    { case cond ~ body => WhileExpr(cond, body) }

  def arrayExpr: Parser[ArrayExpr] = "[" ~> repsep(expr, ",") <~ "]" ^^
    { case arr => ArrayExpr(arr) }

  def debugger: Parser[Debugger] = "debugger" ^^ { case _ => Debugger() }
}
  