type event
type events = event Sel.Event.t list

val lsp : event Sel.Event.t

val handle_event : event -> events