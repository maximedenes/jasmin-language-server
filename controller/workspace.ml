open Lsp.Types
open Jasminlsp
open Language

type root_doc = {
  prog : Typing.program option;
  global_env : Typing.global_env;
  architecture : Jasmin.Glob_options.architecture;
}

type workspace = {
  project_data: ProjectFile.project_data option;
  diagnostics : Diagnostic.t list PathMap.t;
  references : References.reference_map;
  open_documents : DocumentManager.state PathMap.t;
  root_documents : root_doc PathMap.t;
  revdeps : string PathMap.t;
}

let empty_workspace = {
  project_data = None;
  diagnostics = PathMap.empty;
  references = References.empty_reference_map;
  open_documents = PathMap.empty;
  root_documents = PathMap.empty;
  revdeps = PathMap.empty;
}

let find_files ~root acc =
  let rec explore acc = function
    | [] -> acc
    | hd :: tl when Sys.is_directory hd ->
      let files = List.map (Filename.concat hd) @@ Array.to_list (Sys.readdir hd) in
      explore acc (files @ tl)
    | hd :: tl when Filename.extension hd = ".jazz" && not (PathMap.mem hd acc) ->
      let architecture = Jasmin.Glob_options.ARM_M4 in
      explore (PathMap.add hd architecture acc) tl
    | _ :: tl -> explore acc tl
  in
  explore acc [root]

let add_parsing_diagnostics workspace diagnostics =
  let add fname diags =
    match PathMap.find_opt fname workspace.open_documents with (* FIXME what about parsing errors on non opened files? Are they considered part of typing errors? *)
    | None -> diags
    | Some st -> DocumentManager.parsing_diagnostics st @ diags
  in
  PathMap.mapi add diagnostics

let analyze_file fname architecture workspace =
  Printf.eprintf "Analyzing file %s\n" fname;
  let get_ast ~fname = Option.bind (PathMap.find_opt fname workspace.open_documents) (fun st -> DocumentManager.get_ast st) in 
  let Typing.{ diagnostics; references; global_env; program; revdeps } = Typing.type_program get_ast ~fname architecture in
  let diagnostics = add_parsing_diagnostics workspace diagnostics in
  let diagnostics = PathMap.union (fun _ v _ -> Some v) diagnostics workspace.diagnostics in
  let root_doc = { global_env; prog = program; architecture } in
  let root_documents = PathMap.add fname root_doc workspace.root_documents in
  let revdeps = PathMap.union (fun _ v _ -> Some v) revdeps workspace.revdeps in
  { workspace with diagnostics; references; root_documents; revdeps }

let init ~root =
  let path = Lsp.Uri.to_path root in
  let project_file_name = Filename.concat path "jasmin-project.json" in
  let project_data = if Sys.file_exists project_file_name then
    try
      let open ProjectFile in
      let data = parse_project_file ~fname:project_file_name in
      Jasmin.Glob_options.idirs := (data.project_name, data.sources.root)::!Jasmin.Glob_options.idirs; (* FIXME do not rely on glob options *)
      Some data
    with Ppx_yojson_conv_lib.Yojson_conv.Of_yojson_error (_,_) ->
      Format.eprintf "Error parsing jasmin project file\n";
      None
  else None
  in
  let root_path, modules = match project_data with
    | Some { sources = { root; modules } } ->
      let modules = match modules with None -> [] | Some l -> l in
      Filename.concat path root, modules
    | None -> path, []
  in
  let root_files = List.fold_left (fun acc source_module -> PathMap.add source_module.ProjectFile.path source_module.architecture acc) PathMap.empty modules in
  let root_files = find_files ~root:root_path root_files in
  let workspace = 
  {
  project_data;
  diagnostics = PathMap.empty;
  references = References.empty_reference_map;
  open_documents = PathMap.empty;
  root_documents = PathMap.empty;
  revdeps = PathMap.empty;
  }
  in
  Format.eprintf "Analyzing files from workspace\n";
  PathMap.fold analyze_file root_files workspace

let open_document workspace ~fname ~text =
  let doc = DocumentManager.init ~fname ~text in
  let open_documents = PathMap.add fname doc workspace.open_documents in
  let workspace = { workspace with open_documents } in
  match PathMap.find_opt fname workspace.revdeps with
  | None -> Printf.eprintf "Cannot find root document for %s\n" fname; workspace
  | Some root_fname ->
    Printf.eprintf "Opening %s, found root %s\n" fname root_fname;
    match PathMap.find_opt root_fname workspace.root_documents with
    | Some root_doc -> analyze_file root_fname root_doc.architecture workspace
    | None -> Printf.eprintf "Cannot find root document %s\n" root_fname; workspace

let get_document workspace ~fname =
  PathMap.find fname workspace.open_documents

let close_document workspace ~fname =
  let open_documents = PathMap.remove fname workspace.open_documents in
  { workspace with open_documents }

let get_diagnostics workspace =
  workspace.diagnostics

let goto_definition workspace ~fname pos =
  match PathMap.find_opt fname workspace.revdeps with
  | None -> Printf.eprintf "Cannot find root document for %s\n" fname; None
  | Some root_fname ->
    Printf.eprintf "Opening %s, found root %s\n" fname root_fname;
    match PathMap.find_opt root_fname workspace.root_documents with
    | Some root_doc ->
      Typing.find_definition ~fname root_doc.global_env workspace.references pos
    | None -> Printf.eprintf "Cannot find root document %s\n" root_fname; None
