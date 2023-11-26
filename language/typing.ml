open Lsp.Types
open Jasminlsp
open LspData

module type ArchCoreWithAnalyze = sig
  module C : Jasmin.Arch_full.Core_arch
end

open Jasmin.Pretyping

type global_env =
  GlobEnv : 'asm Jasmin.Pretyping.Env.env -> global_env

type program =
  Program : ('asm, 'info) Jasmin.Prog.pprog -> program

type typing_result = {
  diagnostics : Diagnostic.t list PathMap.t;
  references : References.reference_map;
  global_env : global_env;
  program : program option;
  revdeps : string PathMap.t;
}

let push_diag fname diag diagnostics =
  let update = function None -> Some [diag] | Some l -> Some (diag::l) in
  PathMap.update fname update diagnostics

let rec type_item get_ast arch_info (env, diagnostics, revdeps) pt ~root_fname =
  let check pt =
    let open Jasmin.Syntax in
    match Jasmin.Location.unloc pt with
    | PParam  pp -> tt_param  arch_info.pd env (L.loc pt) pp, diagnostics, revdeps
    | PFundef pf -> tt_fundef arch_info env (L.loc pt) pf, diagnostics, revdeps
    | PGlobal pg -> tt_global arch_info.pd env (L.loc pt) pg, diagnostics, revdeps
    | Pexec   pf ->
      Env.Exec.push (L.loc pt) (fst (tt_fun env pf.pex_name)).Jasmin.Prog.f_name pf.pex_mem env, diagnostics, revdeps
    | Prequire (from, fs) -> 
      List.fold_left (fun acc req -> type_file_loc get_ast arch_info from acc ~root_fname req) (env, diagnostics, revdeps) fs
  in
  try
    check pt
  with
  | Jasmin.Pretyping.TyError (loc, code) ->
    let range = Range.of_jasmin_loc loc in
    let buf = Buffer.create 128 in
    let fmt = Format.formatter_of_buffer buf in
    Jasmin.Pretyping.pp_tyerror fmt code;
    Format.pp_print_flush fmt ();
    let message = Buffer.contents buf in
    let diag = Diagnostic.create ~range ~message ~severity:DiagnosticSeverity.Error () in
    let diagnostics = push_diag loc.loc_fname diag diagnostics in
    (env, diagnostics, revdeps)

and type_file_loc get_ast arch_info from env ~root_fname req =
  let loc = Jasmin.Location.loc req in
  let fname = Jasmin.Location.unloc req in
  fst (type_file get_ast arch_info env from (Some loc) ~root_fname ~fname)

and type_file get_ast arch_info (env, diagnostics, revdeps) from loc ~root_fname ~fname =
  match Env.enter_file env from loc fname with
  | None -> (env, diagnostics, revdeps), [] (* already checked *)
  | Some(env, fname) ->
    let revdeps = PathMap.add fname root_fname revdeps in
    try
      let ast = match get_ast ~fname with (* FIXME add parsing diags here *)
        | None ->
          let ast = Parsing.Parse.parse_program ~fname in
          let _,_,ast =  BatFile.with_file_in fname ast in ast
        | Some ast -> ast
      in
      let diagnostics = PathMap.add fname [] diagnostics in
      let (env, diagnostics, revdeps) = List.fold_left (type_item get_ast arch_info ~root_fname) (env, diagnostics, revdeps) ast in
      (Env.exit_file env, diagnostics, revdeps), ast
    with Sys_error message ->
      let diagnostics = match loc with
      | None -> diagnostics
      | Some loc ->
        let fname = loc.loc_fname in
        let range = Range.of_jasmin_loc loc in
        let diag = Diagnostic.create ~range ~message ~severity:DiagnosticSeverity.Error () in
        push_diag fname diag diagnostics
      in
      (Env.exit_file env, diagnostics, revdeps), []

let range_of_error_loc default_fname = function
  | Jasmin.Utils.Lnone ->
    let start = Position.create ~character:0 ~line:0 in
    let end_ = Position.create ~character:0 ~line:0 in
    default_fname, Range.create ~start ~end_
  | Jasmin.Utils.Lone loc -> loc.loc_fname, Range.of_jasmin_loc loc
  | Jasmin.Utils.Lmore iloc -> iloc.base_loc.loc_fname, Range.of_jasmin_loc iloc.base_loc

let push_error_diag fname diagnostics e =
  let buf = Buffer.create 128 in
  let fmt = Format.formatter_of_buffer buf in
  e.Jasmin.Utils.err_msg fmt;
  Format.pp_print_flush fmt ();
  let message = Buffer.contents buf in
  let fname, range = range_of_error_loc fname e.Jasmin.Utils.err_loc in
  let diag = Diagnostic.create ~range ~message ~severity:DiagnosticSeverity.Error () in
  push_diag fname diag diagnostics

let type_program get_ast ~fname target_arch =
  let (module P : ArchCoreWithAnalyze) =
    match target_arch with
    | Jasmin.Glob_options.X86_64 ->
       (module struct
          module C = (val Jasmin.CoreArchFactory.core_arch_x86 ~use_lea:!Jasmin.Glob_options.lea ~use_set0:!Jasmin.Glob_options.set0 !Jasmin.Glob_options.call_conv)
        end)
    | ARM_M4 ->
       (module struct
          module C = Jasmin.CoreArchFactory.Core_arch_ARM
        end)
  in
  let module Arch = Jasmin.Arch_full.Arch_from_Core_arch (P.C) in
  let env =
    List.fold_left Jasmin.Pretyping.Env.add_from Jasmin.Pretyping.Env.empty
      !Jasmin.Glob_options.idirs (* FIXME do not rely on glob options *)
  in
  Jasmin.Glob_options.target_arch := target_arch; 
  let diagnostics = PathMap.singleton fname [] in
  let revdeps = PathMap.empty in
  let (env, diagnostics, revdeps), _ast = type_file get_ast Arch.arch_info (env, diagnostics, revdeps) None None ~root_fname:fname ~fname in
  let pprog = Jasmin.Pretyping.Env.decls env in
  let references = References.collect_references pprog in (* FIXME do this analysis on ast, before typing *)
  begin try
    let prog = Jasmin.Compile.preprocess Arch.reg_size Arch.asmOp pprog in
    let cprog = Jasmin.Conv.cuprog_of_prog prog in
    let visit_prog_after_pass ~debug s p =
      if debug then () else ();
      let open Jasmin in
      if s == Compiler.Unrolling then CheckAnnot.check_no_for_loop p;
      if s == Compiler.Unrolling then CheckAnnot.check_no_inline_instr p
    in
    let diagnostics =
      match Jasmin.Compile.compile (module Arch) visit_prog_after_pass prog cprog with
      | Jasmin.Utils0.Error e ->
        let e = Jasmin.Conv.error_of_cerror (Jasmin.Printer.pp_err ~debug:false) e in
        push_error_diag fname diagnostics e
      | Jasmin.Utils0.Ok _ -> diagnostics
      | exception (Jasmin.Utils.HiError e) ->
        push_error_diag fname diagnostics e
    in
    {
      diagnostics;
      references;
      global_env = GlobEnv env;
      program = Some (Program pprog);
      revdeps;
    }
  with Jasmin.Typing.TyError(loc, message) ->
    let range = Range.of_jasmin_loc loc.base_loc in
    let fname = loc.base_loc.loc_fname in
    let diag = Diagnostic.create ~range ~message ~severity:DiagnosticSeverity.Error () in
    let diagnostics = PathMap.singleton fname [diag] in
    {
      diagnostics;
      references;
      global_env = GlobEnv env;
      program = None;
      revdeps;
    }
  end

let find_definition ~fname global_env references pos =
  let GlobEnv env = global_env in
  References.find_definition env references ~fname pos