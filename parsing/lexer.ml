type triple = Jasmin.Parser.token * Lexing.position * Lexing.position

let buffer =
  ref []

let size =
  ref 0

let more = ref (fun () -> assert false)

let initialize lexbuf =
  more := Jasmin.Parser.MenhirInterpreter.lexer_lexbuf_to_supplier Jasmin.Lexer.main lexbuf;
  buffer := [];
  size := 0

type t = int

let start = 0

let get pos =
  List.nth !buffer (!size - pos)

let token_of_ptoken (p, _, _) = p

(*
let current_position_of_ptoken (_, start, stop) =
  Position.lex_join start stop

let current_position pos =
  current_position_of_ptoken (get pos)
  *)

let get' pos =
  token_of_ptoken (get pos)

let next pos =
  if pos >= !size - 1 then (
    buffer := !more () :: !buffer;
    incr size;
  );
  let pos = pos + 1 in
  (get pos, pos)

let skip_until_before pred pos =
  let rec aux pos =
    let token, _, _ = get pos in
    if token = Jasmin.Parser.EOF then pos
    else if pred token then pos - 1
    else aux (snd (next pos))
  in
  aux pos

let lex_until_before pred pos =
  let rec aux acc ((token,_,_ as triple),pos) =
    if token = Jasmin.Parser.EOF then List.rev (triple::acc), pos
    else if pred token then List.rev acc, pos - 1
    else aux (triple::acc) (next pos)
  in
  aux [] (get pos, pos)