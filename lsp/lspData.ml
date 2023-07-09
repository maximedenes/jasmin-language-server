module Position = struct
  
  include Lsp.Types.Position

  let compare pos1 pos2 =
    match Int.compare pos1.line pos2.line with
    | 0 -> Int.compare pos1.character pos2.character
    | x -> x

  let to_string pos = Format.sprintf "(%i,%i)" pos.line pos.character

end

module Range = struct

  include Lsp.Types.Range

  let of_jasmin_loc Jasmin.Location.{ loc_start; loc_end } =
    let (start_line, start_char) = loc_start in
    let (end_line, end_char) = loc_end in
    let start = Position.{ line = start_line-1; character = start_char; } in
    let end_ = Position.{ line = end_line-1; character = end_char; } in
    { start; end_}

end 

module Location = struct

  include Lsp.Types.Location

  let of_jasmin_loc loc =
    let uri = Lsp.Uri.of_path loc.Jasmin.Location.loc_fname in
    let (l1, c1) = loc.Jasmin.Location.loc_start in
    let (l2, c2) = loc.Jasmin.Location.loc_end in
    let start = Lsp.Types.Position.{ line = l1; character = c1} in
    let end_ = Lsp.Types.Position.{ line = l2; character = c2} in
    let range = Lsp.Types.Range.{ start; end_ } in
    { uri; range }

end

module Settings = struct

  module DelegationMode = struct

  type t = 
    | None
    | Skip 
    | Delegate 

  let yojson_of_t = function
  | None -> `String "None"
  | Skip -> `String "Skip"
  | Delegate -> `String "Delegate"

  let t_of_yojson = function
  | `String "None" -> None
  | `String "Skip" -> Skip
  | `String "Delegate" -> Delegate
  | _ -> Yojson.json_error "invalid value"

  end
  
  module Mode = struct

    type t =
    | Continuous 
    | Manual
    [@@deriving yojson]
      
    let yojson_of_t = function
    | Manual -> `Int 0
    | Continuous -> `Int 1
  
    let t_of_yojson = function
    | `Int 0 -> Manual
    | `Int 1 -> Continuous
    | _ -> Yojson.json_error @@ "invalid value "
  
  end

  module Proof = struct

    type t = {
      delegation: DelegationMode.t;
      workers: int option;
      mode: Mode.t;
    } [@@deriving yojson] [@@yojson.allow_extra_fields]

  end

  type t = {
    proof: Proof.t;
  } [@@deriving yojson] [@@yojson.allow_extra_fields]

end