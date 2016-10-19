case class Scope(var varsDefined: Set[String], var varsUsed: Set[String], parent: Scope = null)

case class SymbolTable(var scope: Scope) {
  def enter {
    scope = Scope(Set(), Set(), scope)
  }

  def exit: Set[String] = {
    val nonLocals = scope.varsUsed -- scope.varsDefined
    scope = scope.parent
    nonLocals
  }

  def defined(name: String) {
    scope.varsDefined += name
  }

  def used(name: String) {
    scope.varsUsed += name
  }
}
