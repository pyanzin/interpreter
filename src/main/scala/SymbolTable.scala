case class Scope(var names: Set[String], parent: Scope = null)

class SymbolTable(var Scope scope) {
    def enter {
        scope = Scope(Set(), scope)
    }

    def exit {
        scope = scope.parent
    }

    def isLocal(id: String) = names(id)
}
