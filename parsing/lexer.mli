type t
type triple = Jasmin.Parser.token * Lexing.position * Lexing.position
val initialize : Lexing.lexbuf -> unit
val start : t
val next  : t -> triple * t
val get   : t -> triple
val get'  : t -> Jasmin.Parser.token
(*
val current_position : t -> Position.t
*)
val skip_until_before : (Jasmin.Parser.token -> bool) -> t -> t
val lex_until_before : (Jasmin.Parser.token -> bool) -> t -> triple list * t