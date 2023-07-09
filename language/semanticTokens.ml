let token_types = ["function"; "keyword"]

let token_modifiers = []

let _log msg = Format.eprintf "       [%d, %f] %s" (Unix.getpid ()) (Unix.gettimeofday ()) msg

let push_token tokens ~deltaLine ~deltaStart ~length ~tokenType ~tokenModifiers =
  tokenModifiers :: tokenType :: length :: deltaStart :: deltaLine :: tokens

let delta_loc lastLine lastStart loc =
  let newLine, newStart = loc.Jasmin.Location.loc_start in
  let newLine = newLine - 1 in
  let deltaLine = newLine - lastLine in
  let deltaStart = if deltaLine > 0 then newStart else newStart - lastStart in
  (deltaLine, deltaStart, newLine, newStart)

let is_keyword (token : Jasmin.Parser.token) =
  BatHashtbl.exists (fun _ t -> t = token) Jasmin.Lexer.keywords

let compute_token (lastLine,lastStart,tokens) node =
  (* log ("cst = " ^ Syntax.Concrete.show_tree node); *)
  let open Parsing.Syntax.Concrete in
  let Jasmin.Location.{ pl_loc; pl_desc = green } = node.green in
  let length = pl_loc.loc_echar - pl_loc.loc_bchar in
  let parent_green = Option.bind node.top (fun x -> Some x.green.pl_desc) in
  let (deltaLine, deltaStart, newLine, newStart) = delta_loc lastLine lastStart pl_loc in
  match green, parent_green with
  | Terminal (Jasmin.Parser.NID _), Some (NonTerminal { kind = (X Jasmin.Parser.MenhirInterpreter.N_pfundef) }) -> 
    newLine, newStart, push_token tokens ~deltaLine ~deltaStart ~length ~tokenType:0 ~tokenModifiers:0
  | Terminal token, _ when is_keyword token ->
    newLine, newStart, push_token tokens ~deltaLine ~deltaStart ~length ~tokenType:1 ~tokenModifiers:0
  | _ -> lastLine, lastStart, tokens

let compute_tokens cst =
  let _,_,tokens = Parsing.Syntax.Concrete.fold compute_token (0,0,[]) cst in
  List.rev tokens