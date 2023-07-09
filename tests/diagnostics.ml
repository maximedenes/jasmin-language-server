open Base
open Controller
open Language
open Lsp
open Jasminlsp

let uri = Uri.of_path "test_file.jazz"
let fname = "test_file.jazz"
let architecture = Jasmin.Glob_options.X86_64

let %test_unit "diagnostics: parsing error is detected" =
  let text = "fn foo(reg u64 bar) -> reg u64 { reg u4 r; return r; }" in
  let doc = DocumentManager.init ~fname ~text in
  let parse_diags = DocumentManager.parsing_diagnostics doc in
  [%test_eq: int] (List.length parse_diags) 1

let %test_unit "diagnostics: typing error is detected" =
  let text = "fn foo(reg u64 bar) -> reg u32 { reg u64 r; return r; }" in
  let doc = DocumentManager.init ~fname ~text in
  let fname' = fname in
  let get_ast ~fname = if String.equal fname fname' then DocumentManager.get_ast doc else None in
  let result = Typing.type_program get_ast architecture ~fname in
  let diags = PathMap.find_opt fname result.diagnostics in
  [%test_eq: int option] (Option.map diags ~f:List.length) (Some 1)

let %test_unit "diagnostics: errors are combined correctly" =
  let text = "fn foo(reg u64 bar) -> reg u32 { reg u64 r; reg baz; return r; }" in
  let workspace = ref Workspace.empty_workspace in
  for _i = 1 to 2 do
  workspace := Workspace.open_document !workspace ~fname ~text;
  workspace := Workspace.analyze_file fname Jasmin.Glob_options.X86_64 !workspace;
  let diags = Workspace.get_diagnostics !workspace in
  [%test_eq: int] (List.length (PathMap.bindings diags)) 1;
  let diags = PathMap.find_opt fname diags in
  [%test_eq: int option] (Option.map diags ~f:List.length) (Some 2)
  done

let %test_unit "diagnostics: parsing errors are resolved correctly" =
  let text = "fn foo(reg u64 bar) -> reg u32 { reg u64 r; reg baz; return r; }" in
  let workspace = Workspace.empty_workspace in
  let workspace = Workspace.open_document workspace ~fname ~text in
  let workspace = Workspace.analyze_file fname Jasmin.Glob_options.X86_64 workspace in
  let text = "fn foo(reg u64 bar) -> reg u32 { reg u64 r; reg u64 baz; return r; }" in
  let workspace = Workspace.open_document workspace ~fname ~text in
  let diags = Workspace.get_diagnostics workspace in
  [%test_eq: int] (List.length (PathMap.bindings diags)) 1;
  let diags = PathMap.find_opt fname diags in
  [%test_eq: int option] (Option.map diags ~f:List.length) (Some 1)

let %test_unit "diagnostics: typing errors are resolved correctly" =
  let text = "fn foo(reg u64 bar) -> reg u32 { reg u64 r; reg baz; return r; }" in
  let workspace = Workspace.empty_workspace in
  let workspace = Workspace.open_document workspace ~fname ~text in
  let text = "fn foo(reg u64 bar) -> reg u64 { reg u64 r; reg baz; return r; }" in
  let workspace = Workspace.analyze_file fname Jasmin.Glob_options.X86_64 workspace in
  let workspace = Workspace.open_document workspace ~fname ~text in
  let workspace = Workspace.analyze_file fname Jasmin.Glob_options.X86_64 workspace in
  let diags = Workspace.get_diagnostics workspace in
  [%test_eq: int] (List.length (PathMap.bindings diags)) 1;
  let diags = PathMap.find_opt fname diags in
  [%test_eq: int option] (Option.map diags ~f:List.length) (Some 1)

let %test_unit "diagnostics: all errors are resolved correctly" =
  let text = "fn foo(reg u64 bar) -> reg u32 { reg u64 r; reg baz; return r; }" in
  let workspace = Workspace.empty_workspace in
  let workspace = Workspace.open_document workspace ~fname ~text in
  let workspace = Workspace.analyze_file fname Jasmin.Glob_options.X86_64 workspace in
  let text = "fn foo(reg u64 bar) -> reg u64 { reg u64 r; reg u64 baz; return r; }" in
  let workspace = Workspace.open_document workspace ~fname ~text in
  let workspace = Workspace.analyze_file fname Jasmin.Glob_options.X86_64 workspace in
  let diags = Workspace.get_diagnostics workspace in
  [%test_eq: int] (List.length (PathMap.bindings diags)) 1;
  let diags = PathMap.find_opt fname diags in
  [%test_eq: int option] (Option.map diags ~f:List.length) (Some 0)