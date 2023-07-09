(** This module implements the entry point of the Jasmin language server *)
open Controller

let log msg = Format.eprintf "              [%d, %f] %s" (Unix.getpid ()) (Unix.gettimeofday ()) msg

let loop () =
  let rec loop (todo : LspManager.event Sel.Todo.t) =
    flush_all ();
    let ready, todo = Sel.pop todo in
    let new_events = LspManager.handle_event ready in
    let todo = Sel.Todo.add todo new_events in
    loop todo
  in
  let todo = Sel.Todo.add Sel.Todo.empty [LspManager.lsp] in
  try loop todo
  with exn ->
    log @@ "Exception raised: " ^ (Printexc.to_string exn)

let _ =
  Sys.(set_signal sigint Signal_ignore);
  loop ()
