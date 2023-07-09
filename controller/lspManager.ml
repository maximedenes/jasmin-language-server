open Jasminlsp

let workspace = ref Workspace.empty_workspace

let server_info = Lsp.Types.InitializeResult.create_serverInfo
  ~name:"jasmin-language-server"
  ~version: "0.0.1"
  ()

let log msg = Format.eprintf "       [%d, %f] %s\n" (Unix.getpid ()) (Unix.gettimeofday ()) msg

type lsp_event = 
  | JsonRpc of Jsonrpc.Packet.t option

type event =
 | LspManagerEvent of lsp_event

type events = event Sel.Event.t list

let lsp : event Sel.Event.t =
  Sel.On.httpcle Unix.stdin (function
    | Ok buff ->
      begin
        log "UI req ready";
        try LspManagerEvent (JsonRpc (Some (Jsonrpc.Packet.t_of_yojson (Yojson.Safe.from_string (Bytes.to_string buff)))))
        with _exn ->
          log @@ "failed to decode json";
          LspManagerEvent (JsonRpc None)
      end
    | Error exn ->
        log @@ ("failed to read message: " ^ Printexc.to_string exn);
        exit 0)

let output_json obj =
  let msg  = Yojson.Safe.pretty_to_string ~std:true obj in
  let size = String.length msg in
  let s = Printf.sprintf "Content-Length: %d\r\n\r\n%s" size msg in
  log @@ "sent: " ^ msg;
  ignore(Unix.write_substring Unix.stdout s 0 (String.length s)) (* TODO ERROR *)

let do_initialize params =
  begin match params.Lsp.Types.InitializeParams.rootUri with
  | None -> ()
  | Some uri ->
    workspace := Workspace.init ~root:uri
  end;
  let open Lsp.Types in
  let legend = SemanticTokensLegend.{
    tokenTypes = Language.SemanticTokens.token_types;
    tokenModifiers = Language.SemanticTokens.token_modifiers;
  }
  in
  let semanticTokensOptions = SemanticTokensOptions.{
    legend;
    range = None;
    full = None;
    workDoneProgress = None;
  }
  in
  let jazz_pattern = FileOperationPattern.{
    glob = "**/*.jazz";
    matches = None;
    options = None;
  }
  in
  let jinc_pattern = FileOperationPattern.{
    glob = "**/*.jinc";
    matches = None;
    options = None;
  }
  in
  let jazz_filter = FileOperationFilter.{
    scheme = None;
    pattern = jazz_pattern;
  }
  in
  let jinc_filter = FileOperationFilter.{
    scheme = None;
    pattern = jinc_pattern;
  }
  in
  let filters = [jazz_filter; jinc_filter] in
  let fileOperationRegistrationOptions = FileOperationRegistrationOptions.{
    filters;
  }
  in
  let didCreate = fileOperationRegistrationOptions in
  let willCreate = fileOperationRegistrationOptions in
  let didRename = fileOperationRegistrationOptions in
  let willRename = fileOperationRegistrationOptions in
  let didDelete = fileOperationRegistrationOptions in
  let willDelete = fileOperationRegistrationOptions in
  let fileOperations = (FileOperationOptions.create
    ~didCreate ~willCreate ~didRename ~willRename ~didDelete ~willDelete ())
  in
  let textDocumentSync = `TextDocumentSyncKind TextDocumentSyncKind.Full in
  let completionProvider = CompletionOptions.create () in 
  let hoverProvider = `Bool true in
  let semanticTokensProvider = `SemanticTokensOptions semanticTokensOptions in
  let definitionProvider = `Bool true in
  let workspace = ServerCapabilities.create_workspace ~fileOperations () in
  let capabilities = Lsp.Types.ServerCapabilities.create
    ~textDocumentSync ~completionProvider ~hoverProvider ~semanticTokensProvider ~definitionProvider ~workspace ()
  in
  let result = Lsp.Types.InitializeResult.{
    capabilities = capabilities; 
    serverInfo = Some server_info;
  }
  in
  Ok result, []

let do_semanticsTokensFull params =
  let uri = params.Lsp.Types.SemanticTokensParams.textDocument.uri in
  let fname = Lsp.Uri.to_path uri in
  let doc = Workspace.get_document !workspace ~fname in
  let data = match (DocumentManager.concrete_syntax_tree doc) with
    | None -> log "semantic tokens requested but no cst"; [||]
    | Some cst -> Array.of_list @@ Language.SemanticTokens.compute_tokens cst
  in
  let result = Lsp.Types.SemanticTokens.create ~data () in
  Ok (Some result), []

let do_definition params =
  let uri = params.Lsp.Types.DefinitionParams.textDocument.uri in
  let pos = params.Lsp.Types.DefinitionParams.position in
  let fname = Lsp.Uri.to_path uri in
  let result = Workspace.goto_definition !workspace ~fname pos in
  Ok (Some (`Location (Option.to_list result))), []

let do_shutdown () =
  Ok (), []

let publish_diagnostics fname diagnostics =
  let uri = Lsp.Uri.of_path fname in
  let params = Lsp.Types.PublishDiagnosticsParams.create ~diagnostics ~uri () in
  let notification = Lsp.Server_notification.PublishDiagnostics params in
  output_json @@ Jsonrpc.Notification.yojson_of_t @@ Lsp.Server_notification.to_jsonrpc notification

let publish_all_diagnostics () =
  PathMap.iter publish_diagnostics @@ Workspace.get_diagnostics !workspace

let textDocumentDidOpen params =
  let Lsp.Types.DidOpenTextDocumentParams.{ textDocument } = params in
  let fname = Lsp.Uri.to_path textDocument.uri in
  workspace := Workspace.open_document !workspace ~fname ~text:textDocument.text;
  publish_all_diagnostics (); (* FIXME restrict to changed diagnostics *)
  []

let textDocumentDidChange params =
  let Lsp.Types.DidChangeTextDocumentParams.{ textDocument; contentChanges } = params in
  begin match contentChanges with
  | [change] ->
    let fname = Lsp.Uri.to_path textDocument.uri in
    workspace := Workspace.open_document !workspace ~fname ~text:change.text;
    publish_all_diagnostics (); (* FIXME restrict to changed diagnostics *)
  | _ -> ()
  end;
  []

let textDocumentDidClose params =
  let Lsp.Types.DidCloseTextDocumentParams.{ textDocument } = params in
  let fname = Lsp.Uri.to_path textDocument.uri in
  workspace := Workspace.close_document !workspace ~fname;
  []

(*
let textDocumentDidChange params =
  let open Yojson.Safe.Util in
  let textDocument = params |> member "textDocument" in
  let uri = textDocument |> member "uri" |> to_string in
  let contentChanges = params |> member "contentChanges" |> to_list in
  let read_edit edit =
    let text = edit |> member "text" |> to_string in
    let range = Range.t_of_yojson (edit |> member "range") in
    range, text
  in
  let textEdits = List.map read_edit contentChanges in
  let st = Hashtbl.find states uri in
  let st = Dm.DocumentManager.apply_text_edits st textEdits in
  let (st, events) = 
    if !check_mode = Settings.Mode.Continuous then 
      Dm.DocumentManager.interpret_to_end st 
    else 
      (st, [])
  in
  Hashtbl.replace states uri st;
  publish_diagnostics uri st;
  uri, events

let textDocumentHover ~id params = 
  let open Yojson.Safe.Util in
  let textDocument = params |> member "textDocument" in
  let uri = textDocument |> member "uri" |> to_string in
  let loc = params |> member "position" |> parse_loc in 
  let st = Hashtbl.find states uri in 
  match Dm.DocumentManager.hover st loc with
  (* Response: result: Hover | null *)
  | None -> output_json @@ Response.(yojson_of_t { id; result = Ok (`Null) })
  | Some (Ok contents) ->
    let result = Ok (Hover.(yojson_of_t {contents})) in
    output_json @@ Response.(yojson_of_t { id; result })
  | _ -> ()
  *)

let dispatch_request : type a. a Lsp.Client_request.t -> (a, string) result * events =
  fun req ->
  match req with
  | Initialize params ->
    do_initialize params (* TODO send initial diagnostics *)
  | SemanticTokensFull params ->
    do_semanticsTokensFull params
  | TextDocumentDefinition params ->
    do_definition params
  | Shutdown ->
    do_shutdown ()
  | _ -> Error "Received unknown request", []

let dispatch_notification =
  let open Lsp.Client_notification in function
  | TextDocumentDidOpen params ->
    textDocumentDidOpen params
  | TextDocumentDidChange params ->
    textDocumentDidChange params
  | TextDocumentDidClose params ->
    textDocumentDidClose params
  | _ -> []

let handle_lsp_event = function
  | JsonRpc None ->
      [lsp]
  | JsonRpc (Some rpc) ->
    lsp ::
    match rpc with
    | Request req ->
        log @@ "ui request: " ^ req.method_;
        begin match Lsp.Client_request.of_jsonrpc req with
          | Error message ->
            log @@ "Error decoding request: " ^ message; []
          | Ok(E r) ->
            let resp, events = dispatch_request r in
            begin match resp with
            | Error message ->
              output_json @@ Jsonrpc.Response.(yojson_of_t @@ error req.id (Error.make ~code:RequestFailed ~message ()))
            | Ok resp ->
              let resp = Lsp.Client_request.yojson_of_result r resp in
              output_json Jsonrpc.Response.(yojson_of_t @@ ok req.id resp)
            end;
            events
        end
    | Notification notif ->
      begin match Lsp.Client_notification.of_jsonrpc notif with
      | Ok notif ->
        dispatch_notification notif
      | Error e -> log @@ "error decoding notification: " ^ e; []
      end
    | Response _resp ->
        log @@ "got unknown response";
        []
    | Batch_response _ -> log "Unsupported batch response received"; []
    | Batch_call _ -> log "Unsupported batch call received"; []

let handle_event = function
  | LspManagerEvent e -> handle_lsp_event e


