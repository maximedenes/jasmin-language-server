module Concrete :
  sig

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
      top: node option;
    }

    val make_terminal :
      Lexing.position -> Lexing.position -> Jasmin.Parser.token -> green

    val make_nonterminal :
      xnonterminal -> green list -> green

    val fold : ('a -> node -> 'a) -> 'a -> node -> 'a

    val fold_skip_errors : ('a -> node -> 'a) -> 'a -> node -> 'a

    val mk_root : green -> node

    (*
    val show_tree : node -> string
    *)

  end
