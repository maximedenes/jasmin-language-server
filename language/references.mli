open Jasminlsp.LspData

type reference_map

val empty_reference_map : reference_map

val collect_references : ('info, 'asm) Jasmin.Prog.pprog -> reference_map

val find_definition : 'asm Jasmin.Pretyping.Env.env -> reference_map -> fname:string -> Lsp.Types.Position.t -> Location.t option