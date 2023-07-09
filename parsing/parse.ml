module L = MenhirLib.LexerUtil
module E = MenhirLib.ErrorReports

module P = Jasmin.Parser

module I = P.MenhirInterpreter

type token = Jasmin.Parser.token =
  | WHILE
  | UNDERSCORE
  | T_U8
  | T_U64
  | T_U32
  | T_U256
  | T_U16
  | T_U128
  | T_INT
  | T_BOOL
  | TRUE
  | TO
  | SWSIZE of (Jasmin.Syntax.swsize) [@printer fun fmt _ -> fprintf fmt "SWSIZE"]
  | SVSIZE of (Jasmin.Syntax.svsize) [@printer fun fmt _ -> fprintf fmt "SVSIZE"]
  | STRING of (string)
  | STAR
  | STACK
  | SLASH
  | SHARP
  | SEMICOLON
  | RPAREN
  | ROR
  | ROL
  | RETURN
  | REQUIRE
  | REG
  | RBRACKET
  | RBRACE
  | RARROW
  | QUESTIONMARK
  | POINTER
  | PLUS
  | PIPEPIPE
  | PIPE
  | PERCENT
  | PARAM
  | NID of (string)
  | MUTABLE
  | MINUS
  | LTLT
  | LT of (Jasmin.Syntax.sign) [@printer fun fmt _ -> fprintf fmt "LT"]
  | LPAREN
  | LE of (Jasmin.Syntax.sign) [@printer fun fmt _ -> fprintf fmt "LE"]
  | LBRACKET
  | LBRACE
  | INT of (Z.t) [@printer fun fmt _ -> fprintf fmt "INT"] 
  | INLINE
  | IF
  | HAT
  | GTGT of (Jasmin.Syntax.sign) [@printer fun fmt _ -> fprintf fmt "GTGT"]
  | GT of (Jasmin.Syntax.sign) [@printer fun fmt _ -> fprintf fmt "GT"]
  | GLOBAL
  | GE of (Jasmin.Syntax.sign) [@printer fun fmt _ -> fprintf fmt "GE"]
  | FROM
  | FOR
  | FN
  | FALSE
  | EXPORT
  | EXEC
  | EQEQ
  | EQ
  | EOF
  | ELSE
  | DOWNTO
  | DOT
  | CONSTANT
  | COMMA
  | COLON
  | BANGEQ
  | BANG
  | ARRAYINIT
  | AMPAMP
  | AMP
[@@deriving show]


(* -------------------------------------------------------------------- *)
open MenhirLib.General
open I

module Printers = struct

  let buf = Buffer.create 16
  let print s = Buffer.add_string buf s

  let print_nt : type a. a nonterminal -> string =
    fun nt ->
    match nt with
    | N_writable -> "writable"
    | N_var -> "var"
    | N_utype -> "utype"
    | N_top_annotation -> "top_annotation"
    | N_top -> "top"
    | N_swsize -> "swsize"
    | N_svsize -> "svsize"
    | N_struct_annot -> "struct_annot"
    | N_storage -> "storage"
    | N_stor_type -> "stor_type"
    | N_simple_attribute -> "simple_attribute"
    | N_separated_nonempty_list_option_COMMA__var_ -> "separated_nonempty_list_option_COMMA__var_"
    | N_separated_nonempty_list_empty_var_ -> "separated_nonempty_list_empty_var_"
    | N_separated_nonempty_list_COMMA_var_ -> "separated_nonempty_list_COMMA_var_"
    | N_separated_nonempty_list_COMMA_range_ -> "separated_nonempty_list_COMMA_range_"
    | N_separated_nonempty_list_COMMA_plvalue_ -> "separated_nonempty_list_COMMA_plvalue_"
    | N_separated_nonempty_list_COMMA_pexpr_ -> "separated_nonempty_list_COMMA_pexpr_"
    | N_separated_nonempty_list_COMMA_annotation_ -> "separated_nonempty_list_COMMA_annotation_"
    | N_separated_nonempty_list_COMMA_annot_stor_type_ -> "separated_nonempty_list_COMMA_annot_stor_type_"
    | N_separated_nonempty_list_COMMA_annot_pvardecl_ -> "separated_nonempty_list_COMMA_annot_pvardecl_"
    | N_range -> "range"
    | N_ptype -> "ptype"
    | N_ptr -> "ptr"
    | N_prim -> "prim"
    | N_prequire1 -> "prequire1"
    | N_prequire -> "prequire"
    | N_pparam -> "pparam"
    | N_pointer -> "pointer"
    | N_plvalues -> "plvalues"
    | N_plvalue -> "plvalue"
    | N_pinstr_r -> "pinstr_r"
    | N_pinstr -> "pinstr"
    | N_pif -> "pif"
    | N_pglobal -> "pglobal"
    | N_pgexpr -> "pgexpr"
    | N_pfundef -> "pfundef"
    | N_pfunbody -> "pfunbody"
    | N_pexpr -> "pexpr"
    | N_pexec -> "pexec"
    | N_peqop -> "peqop"
    | N_pelseif -> "pelseif"
    | N_pelse -> "pelse"
    | N_pblock -> "pblock"
    | N_option_writable_ -> "option_writable_"
    | N_option_utype_ -> "option_utype_"
    | N_option_prefix_RARROW_tuple_annot_stor_type___ -> "option_prefix_RARROW_tuple_annot_stor_type___"
    | N_option_prefix_IF_pexpr__ -> "option_prefix_IF_pexpr__"
    | N_option_pointer_ -> "option_pointer_"
    | N_option_pblock_ -> "option_pblock_"
    | N_option_parens_utype__ -> "option_parens_utype__"
    | N_option_mem_ofs_ -> "option_mem_ofs_"
    | N_option_from_ -> "option_from_"
    | N_option_call_conv_ -> "option_call_conv_"
    | N_option_attribute_ -> "option_attribute_"
    | N_option_arr_access_len_ -> "option_arr_access_len_"
    | N_option_DOT_ -> "option_DOT_"
    | N_option_COMMA_ -> "option_COMMA_"
    | N_nonempty_list_prequire1_ -> "nonempty_list_prequire1_"
    | N_module_ -> "module_"
    | N_loption_separated_nonempty_list_COMMA_var__ -> "loption_separated_nonempty_list_COMMA_var__"
    | N_loption_separated_nonempty_list_COMMA_range__ -> "loption_separated_nonempty_list_COMMA_range__"
    | N_loption_separated_nonempty_list_COMMA_pexpr__ -> "loption_separated_nonempty_list_COMMA_pexpr__"
    | N_loption_separated_nonempty_list_COMMA_annotation__ -> "loption_separated_nonempty_list_COMMA_annotation__"
    | N_loption_separated_nonempty_list_COMMA_annot_stor_type__ -> "loption_separated_nonempty_list_COMMA_annot_stor_type__"
    | N_loption_separated_nonempty_list_COMMA_annot_pvardecl__ -> "loption_separated_nonempty_list_COMMA_annot_pvardecl__"
    | N_list_top_annotation_ -> "list_top_annotation_"
    | N_list_pinstr_ -> "list_pinstr_"
    | N_keyword -> "keyword"
    | N_int -> "int"
    | N_implicites -> "implicites"
    | N_from -> "from"
    | N_castop1 -> "castop1"
    | N_castop -> "castop"
    | N_cast -> "cast"
    | N_call_conv -> "call_conv"
    | N_attribute -> "attribute"
    | N_arr_access_len -> "arr_access_len"
    | N_arr_access_i -> "arr_access_i"
    | N_arr_access -> "arr_access"
    | N_annotations -> "annotations"
    | N_annotationlabel -> "annotationlabel"
    | N_annotation -> "annotation"
    | N_annot_stor_type -> "annot_stor_type"
    | N_annot_pvardecl -> "annot_pvardecl"
    | N_ptype_r -> "ptype_r"
    | N_plvalue_r -> "plvalue_r"
    | N_pexpr_r -> "pexpr_r"
    | N_pblock_r -> "pblock_r"
    | N_option_loc_castop1__ -> "option_loc_castop1"
    | N_option___anonymous_1_ -> "option___anonymous_1"
    | N_list_loc_top__ -> "list_loc_top"

  let print_terminal : type a. a terminal -> string =
    fun t -> match t with
    | T_error -> "error"
    | T_WHILE -> "WHILE"
    | T_UNDERSCORE -> "UNDERSCORE"
    | T_T_U8 -> "T_U8"
    | T_T_U64 -> "T_U64"
    | T_T_U32 -> "T_U32"
    | T_T_U256 -> "T_U256"
    | T_T_U16 -> "T_U16"
    | T_T_U128 -> "T_U128"
    | T_T_INT -> "T_INT"
    | T_T_BOOL -> "T_BOOL"
    | T_TRUE -> "TRUE"
    | T_TO -> "TO"
    | T_SWSIZE -> "SWSIZE"
    | T_SVSIZE -> "SVSIZE"
    | T_STRING -> "STRING"
    | T_STAR -> "STAR"
    | T_STACK -> "STACK"
    | T_SLASH -> "SLASH"
    | T_SHARP -> "SHARP"
    | T_SEMICOLON -> "SEMICOLON"
    | T_RPAREN -> "RPAREN"
    | T_ROR -> "ROR"
    | T_ROL -> "ROL"
    | T_RETURN -> "RETURN"
    | T_REQUIRE -> "REQUIRE"
    | T_REG -> "REG"
    | T_RBRACKET -> "RBRACKET"
    | T_RBRACE -> "RBRACE"
    | T_RARROW -> "RARROW"
    | T_QUESTIONMARK -> "QUESTIONMARK"
    | T_POINTER -> "POINTER"
    | T_PLUS -> "PLUS"
    | T_PIPEPIPE -> "PIPEPIPE"
    | T_PIPE -> "PIPE"
    | T_PERCENT -> "PERCENT"
    | T_PARAM -> "PARAM"
    | T_NID -> "NID"
    | T_MUTABLE -> "MUTABLE"
    | T_MINUS -> "MINUS"
    | T_LTLT -> "LTLT"
    | T_LT -> "LT"
    | T_LPAREN -> "LPAREN"
    | T_LE -> "LE"
    | T_LBRACKET -> "LBRACKET"
    | T_LBRACE -> "LBRACE"
    | T_INT -> "INT"
    | T_INLINE -> "INLINE"
    | T_IF -> "IF"
    | T_HAT -> "HAT"
    | T_GTGT -> "GTGT"
    | T_GT -> "GT"
    | T_GLOBAL -> "GLOBAL"
    | T_GE -> "GE"
    | T_FROM -> "FROM"
    | T_FOR -> "FOR"
    | T_FN -> "FN"
    | T_FALSE -> "FALSE"
    | T_EXPORT -> "EXPORT"
    | T_EXEC -> "EXEC"
    | T_EQEQ -> "EQEQ"
    | T_EQ -> "EQ"
    | T_EOF -> "EOF"
    | T_ELSE -> "ELSE"
    | T_DOWNTO -> "DOWNTO"
    | T_DOT -> "DOT"
    | T_CONSTANT -> "CONSTANT"
    | T_COMMA -> "COMMA"
    | T_COLON -> "COLON"
    | T_BANGEQ -> "BANGEQ"
    | T_BANG -> "BANG"
    | T_ARRAYINIT -> "ARRAYINIT"
    | T_AMPAMP -> "AMPAMP"
    | T_AMP -> "AMP"

  let print_symbol (symbol : xsymbol) =
    let s = match symbol with
    | X (T t) -> print_terminal t
    | X (N nt) -> print_nt nt
    in
    Buffer.add_string buf s

  let print_element = Some (fun _e -> Buffer.add_string buf "EL")

  let reset () = Buffer.reset buf

end

module Print = MenhirLib.Printers.Make(I)(Printers)

let string_of_symbol symb =
  Printers.reset ();
  Printers.print_symbol symb;
  Buffer.contents Printers.buf

type 'a last_reduction =
  | FoundTopAt of 'a
  | FoundInstructionAt of 'a
  | FoundNothingAt of 'a

type 'a recovery_state = {
  last_reduction : 'a last_reduction;
  new_symbols : int;
}

let rec reduce_cst n nt cst acc =
  match cst with
  | [] -> if n > 0 then raise (Failure "More symbols but empty cst")
    else [Syntax.Concrete.make_nonterminal nt acc]
  | Jasmin.Location.{ pl_desc = Syntax.Concrete.NonTerminal { kind = Error } } as symb :: cst' ->
    (* Error nodes should not count as RHS symbols *)
    reduce_cst n nt cst' (symb::acc)
  | symb :: cst' ->
    if n > 0 then reduce_cst (n-1) nt cst' (symb::acc)
    else Syntax.Concrete.make_nonterminal nt acc :: cst

let update_recovery_state_reduce inputneeded production recovery_state =
  match lhs production with
  | X (N N_top) ->
     { last_reduction = FoundTopAt inputneeded; new_symbols = 0 }
  | X (N N_pinstr) ->
     { last_reduction = FoundInstructionAt inputneeded; new_symbols = 0 }
  | _ ->
     { recovery_state with new_symbols = recovery_state.new_symbols - List.length (rhs production) + 1}

let update_recovery_state_input recovery_state =
  { recovery_state with new_symbols = recovery_state.new_symbols + 1 }

let take_symbols n l =
  if n > 0 then match l with
    | [] -> l
    | Jasmin.Location.{ pl_desc = Syntax.Concrete.NonTerminal { kind = Error } } as hd :: tl -> hd :: take n tl
    | hd :: tl -> hd :: take (n-1) tl
  else l

let resume_on_error recovery_state cst extra_tokens lex =
  match recovery_state.last_reduction with
  | FoundInstructionAt (checkpoint, cst') ->
     let extra_tokens', lex =
       Lexer.lex_until_before (fun t -> t = SEMICOLON || t = RBRACE) lex
     in
     let (token,_,_), lex' = Lexer.next lex in
     let lex =
       if token = SEMICOLON then lex' else lex
     in
     let extra = List.rev_map (fun (v,startp,endp) -> Syntax.Concrete.make_terminal startp endp v) extra_tokens' in
     let extra = List.rev_append extra_tokens extra in
     let error_children = List.rev_append (take_symbols recovery_state.new_symbols cst) extra in
     let error = Syntax.Concrete.(make_nonterminal Error error_children) in
     let cst = error :: cst' in
     let recovery_state = { recovery_state with new_symbols = 0 } in
     lex, checkpoint, cst, recovery_state
  | (FoundNothingAt (checkpoint, cst') | FoundTopAt (checkpoint, cst')) ->
     let extra_tokens', lex = Lexer.lex_until_before
        (function EOF | FN | (* SHARP | *) EXPORT | INLINE | PARAM | EXEC | REQUIRE | FROM (* TODO add pglobal *) -> true | _ -> false)
        lex
     in
     let extra = List.rev_map (fun (v,startp,endp) -> Syntax.Concrete.make_terminal startp endp v) extra_tokens' in
     let extra = List.rev_append extra_tokens extra in
     let error_children = List.rev_append (take_symbols recovery_state.new_symbols cst) extra in
     let error = Syntax.Concrete.(make_nonterminal Error error_children) in
     let cst = error :: cst' in
     let recovery_state = { recovery_state with new_symbols = 0 } in
     lex, checkpoint, cst, recovery_state

let extract_nonterminal symb =
  match symb with
  | I.(X T _) -> assert false
  | I.(X N nt) -> Syntax.Concrete.X nt


let rec show_green n g =
  let open Syntax.Concrete in
  match g.Jasmin.Location.pl_desc with
  | Terminal token -> String.make n ' ' ^  show_token token
  | NonTerminal { kind = Error; children } ->
    String.make n ' ' ^  "ERROR\n" ^ String.concat "\n" (List.map (show_green (n+2)) children)
  | NonTerminal { kind = X nt; children } ->
    String.make n ' ' ^ Printers.print_nt nt ^ "\n" ^ String.concat "\n" (List.map (show_green (n+2)) children)

let show_green g = show_green 0 g

let show_tree node =
  let open Syntax.Concrete in
  let show_node x =
    Format.sprintf "%s" (show_green x.green)
  in
  fold (fun acc x -> acc ^ " " ^ show_node x) "" node

let rec loop lexer inputneeded cst recovery_state extra_tokens errors (checkpoint : 'a I.checkpoint) =
  match checkpoint with
  | I.InputNeeded _env ->
      let (token, startp, endp as triple), lexer = Lexer.next lexer in
      let extra_tokens = Syntax.Concrete.make_terminal startp endp token :: extra_tokens in
      let recovery_state = update_recovery_state_input recovery_state in
      loop lexer (checkpoint, cst) cst recovery_state extra_tokens errors (I.offer checkpoint triple)
  | I.Shifting _ ->
      let checkpoint = I.resume checkpoint in
      let cst = List.append extra_tokens cst in
      loop lexer inputneeded cst recovery_state [] errors checkpoint
  | I.AboutToReduce (_env, production) ->
      let n = List.length (rhs production) in
      let nt = extract_nonterminal (lhs production) in
      let cst = reduce_cst n nt cst [] in
      let checkpoint = I.resume checkpoint in
      loop lexer inputneeded cst (update_recovery_state_reduce inputneeded production recovery_state) extra_tokens errors checkpoint
  | I.Rejected -> assert false
  | I.HandlingError env ->
    let lexer, after_error, cst, recovery_state = resume_on_error recovery_state cst extra_tokens lexer in
    let error = positions env in
    loop lexer inputneeded cst recovery_state [] (error::errors) after_error
  | I.Accepted _v ->
    begin match cst with
    | [root] -> Syntax.Concrete.mk_root root, errors
    | _ ->
      List.iter (fun v -> Printf.eprintf "Non-unique root when accepting: %s\n" (show_green v)) cst;
      assert false
    end

let program_cst ~fname (inc : Jasmin.Utils.IO.input) =
  let ch = Jasmin.Utils.IO.to_input_channel inc in
  let lexbuf = L.init fname (Lexing.from_channel ch) in
  Lexer.initialize lexbuf;
  let checkpoint = P.Incremental.module_ lexbuf.lex_curr_p in
  let recovery_state = { last_reduction = FoundNothingAt (checkpoint, []); new_symbols = 0 } in
  loop Lexer.start (checkpoint, []) [] recovery_state [] [] checkpoint

let succeed v = v

let fail buffer _checkpoint =
  let (p1,p2) = E.last buffer in
  let location = Jasmin.Location.make p1 p2 in
  let message = "Parsing error" in
  raise (Jasmin.Syntax.ParseError (location, Some message))

let parse_program_from_tokens startp supplier =
  let buffer, supplier = E.wrap_supplier supplier in
  let checkpoint = P.Incremental.module_ startp in
  I.loop_handle succeed (fail buffer) supplier checkpoint

let pos_of_loc l =
  let open Lexing in
  let open Jasmin.Location in
  let pos_fname = l.loc_fname in
  let (pos_lnum_start, pos_char_start) = l.loc_start in
  let (pos_lnum_end, pos_char_end) = l.loc_end in
  let pos_bol_start = l.loc_bchar - pos_char_start in
  let pos_bol_end = l.loc_echar - pos_char_end in
  let startp = { pos_fname; pos_lnum = pos_lnum_start; pos_cnum = l.loc_bchar; pos_bol = pos_bol_start } in
  let endp = { pos_fname; pos_lnum = pos_lnum_end; pos_cnum = l.loc_echar; pos_bol = pos_bol_end } in
  (startp, endp)

let tokens_of_cst cst =
  Syntax.Concrete.fold_skip_errors (fun acc node -> match node.green.pl_desc with Terminal x -> let (startp,endp) = pos_of_loc node.green.pl_loc in (x, startp, endp) :: acc | _ -> acc) [] cst

let dispenser_of_token_list l =
  let d = Seq.to_dispenser (List.to_seq l) in
  fun () -> Option.get (d ())

let parse_program ~fname inc =
  let cst, errors = program_cst ~fname inc in
  let startp = Lexing.{
    pos_fname = fname;
    pos_lnum  = 1;
    pos_bol   = 0;
    pos_cnum  = 0
  }
  in
  let tokens = List.rev (tokens_of_cst cst) in
  cst, errors, parse_program_from_tokens startp (dispenser_of_token_list tokens)