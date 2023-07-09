open Lsp.Types
open Jasminlsp
open LspData

type global_env
type program

type typing_result = {
  diagnostics : Diagnostic.t list PathMap.t;
  references : References.reference_map;
  global_env : global_env;
  program : program option;
  revdeps : string PathMap.t;
}

val type_program :
  (fname:string -> Jasmin.Syntax.pprogram option) ->
  fname:string ->
  Jasmin.Glob_options.architecture ->
  typing_result

val find_definition : fname:string -> global_env -> References.reference_map -> Lsp.Types.Position.t -> Location.t option