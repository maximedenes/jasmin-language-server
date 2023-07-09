open Jasmin.Parser.MenhirInterpreter

val string_of_symbol : xsymbol -> string

val parse_program_from_tokens : Lexing.position -> (unit -> token * Lexing.position * Lexing.position) -> Jasmin.Syntax.pprogram
val parse_program : fname:string -> Jasmin.Utils.IO.input -> Syntax.Concrete.node * (Lexing.position * Lexing.position) list * Jasmin.Syntax.pprogram

val show_tree : Syntax.Concrete.node -> string