open Jasminlsp
open LspData

module PositionMap = Map.Make(Position)

type reference_r =
  | RefFun of Jasmin.Annotations.symbol

type reference =
  { range : Lsp.Types.Range.t; reference: reference_r }

type reference_map = reference PositionMap.t PathMap.t

let empty_reference_map = PathMap.empty

let positions_of_iloc iloc =
  let loc = iloc.Jasmin.Location.base_loc in
  let (l1, c1) = loc.loc_start in
  let (l2, c2) = loc.loc_end in
  Position.{ line = l1-1; character = c1}, Position.{ line = l2-1; character = c2}

let rec collect_instr_references acc instr =
  match instr.Jasmin.Prog.i_desc with
  | Jasmin.Prog.Cassgn (_, _, _, _) -> acc
  | Jasmin.Prog.Copn (_, _, _, _) -> acc
  | Jasmin.Prog.Csyscall (_, _, _) -> acc
  | Jasmin.Prog.Cif (_, stmt1, stmt2) ->
    let acc = collect_prog_references acc stmt1 in
    collect_prog_references acc stmt2
  | Jasmin.Prog.Cfor (_, _, stmt) ->
    collect_prog_references acc stmt
  | Jasmin.Prog.Cwhile (_, stmt1, _, stmt2) ->
    let acc = collect_prog_references acc stmt1 in
    collect_prog_references acc stmt2
  | Jasmin.Prog.Ccall (_, _, funname, _) ->
    let (start, end_) = positions_of_iloc instr.i_loc in
    let range = Lsp.Types.Range.{ start; end_ } in
    let r = { range; reference = RefFun funname.fn_name } in
    let fname = instr.i_loc.base_loc.loc_fname in
    PathMap.update fname (function None -> Some (PositionMap.singleton start r) | Some map -> Some (PositionMap.add start r map)) acc

and collect_prog_references acc prog =
  List.fold_left collect_instr_references acc prog

let collect_mod_references acc m =
  match m with
  | Jasmin.Prog.MIfun { f_body } -> collect_prog_references acc f_body
  | Jasmin.Prog.MIparam _ -> acc
  | Jasmin.Prog.MIglobal _ -> acc

let collect_references prog =
  List.fold_left collect_mod_references PathMap.empty prog

let find_reference refmap pos =
  Printf.eprintf "Search for reference at line %d char %d\n" pos.Position.line pos.character;
  match PositionMap.find_last_opt (fun start -> Position.compare start pos <= 0) refmap with
  | None -> None
  | Some (_,r) ->
    Printf.eprintf "Found reference close to pos, ends line %d char %d\n" r.range.end_.line r.range.end_.character;
    if Position.compare r.range.end_ pos >= 0 then Some r else None

let find_definition env refmap ~fname pos =
  begin match PathMap.find_opt fname refmap with
  | None -> None
  | Some map ->
    Printf.eprintf "Found map\n";
    begin match find_reference map pos with
    | None -> None
    | Some { reference = RefFun funname } ->
      Printf.eprintf "Found function reference %s\n" funname;
      begin match Jasmin.Pretyping.Env.Funs.find funname env with
      | None -> None
      | Some (func,_ty) -> Some (Location.of_jasmin_loc func.f_loc)
      end
    end
  end