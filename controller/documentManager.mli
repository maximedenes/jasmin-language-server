open Lsp.Types

type state

val init : fname:string -> text:string -> state

val parsing_diagnostics : state -> Diagnostic.t list
val concrete_syntax_tree : state -> Parsing.Syntax.Concrete.node option
val get_ast : state -> Jasmin.Syntax.pprogram option