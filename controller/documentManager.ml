open Lsp.Types
open Jasminlsp.LspData
open Parsing

type state = {
  ast : Jasmin.Syntax.pprogram option;
  cst : Parsing.Syntax.Concrete.node option;
  parsing_diagnostics : Diagnostic.t list;
}

let range_of_lexpos startp endp =
  let open Lexing in
  let start = Position.{ line = startp.pos_lnum-1; character = startp.pos_cnum - startp.pos_bol; } in
  let end_ = Position.{ line = endp.pos_lnum-1; character = endp.pos_cnum - endp.pos_bol; } in
  Range.{ start; end_}

let init ~fname ~text =
  let input = BatIO.input_string text in
  let cst, errors, ast = Parse.parse_program ~fname input in
  let mk_diag (startp, endp) =
    let range = range_of_lexpos startp endp in
    let message = "Parsing error." in
    Diagnostic.create ~range ~message ~severity:DiagnosticSeverity.Error ()
  in
  let parsing_diagnostics = List.map mk_diag errors in
  { parsing_diagnostics; ast = Some ast; cst = Some cst }

let parsing_diagnostics st = st.parsing_diagnostics
let concrete_syntax_tree st = st.cst

let get_ast st = st.ast