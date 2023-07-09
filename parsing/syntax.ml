module Concrete = struct

  type xnonterminal =
    | X : 'a Jasmin.Parser.MenhirInterpreter.nonterminal -> xnonterminal
    | Error : xnonterminal

  type green_r =
  | Terminal of Jasmin.Parser.token
  | NonTerminal of {
      kind : xnonterminal;
      children : green list;
    }

  and green = green_r Jasmin.Location.located

  type node = {
    green: green;
    top: node option; (* TODO can we express that this is not a leaf? *)
  }

  let node_children node =
    let mk_child green =
      { green; top = Some node; }
    in
    match node.green.pl_desc with
    | Terminal _ -> []
    | NonTerminal { children } ->
      List.map mk_child children

  let make_terminal start stop token = Jasmin.Location.(mk_loc @@ make start stop) (Terminal token)

  let make_nonterminal kind children =
    let rec last_stop = function
    | [] -> assert false
    | [x] -> x.Jasmin.Location.pl_loc
    | _ :: tl -> last_stop tl
    in
    let start, stop = match children with
    | [] -> Jasmin.Location._dummy, Jasmin.Location._dummy
    | _ -> (List.hd children).Jasmin.Location.pl_loc, last_stop children
    in
    Jasmin.Location.(mk_loc @@ merge start stop) (NonTerminal { kind; children })

  let rec fold f acc node =
    let acc = f acc node in
    List.fold_left (fold f) acc (node_children node)

  let rec fold_skip_errors f acc node =
    match node.green.pl_desc with
    | NonTerminal { kind = Error } -> acc
    | _ ->
      let acc = f acc node in
      List.fold_left (fold_skip_errors f) acc (node_children node)

  let mk_root green =
    { green; top = None }

end