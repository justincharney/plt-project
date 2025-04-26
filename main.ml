(* main.ml *)
open Ast

let () =
  let lexbuf = Lexing.from_channel stdin in
  try
    (* 1. Lex & parse *)
    let ast = Parser.program Scanner.token lexbuf in

    (* 2. Semantic check & build SAST *)
    let sast = Semantic.analyze ast in

    (* 3. Pretty-print the SAST (you’ll need to write your own printer) *)
    Pat_sast.pp_program sast;
    exit 0

  with
  | SemanticError msg ->
      prerr_endline ("Semantic error: " ^ msg);
      exit 1
  | Scanner.LexError msg ->
      prerr_endline ("Lexing error: " ^ msg);
      exit 1
  | Parser.Error ->
      prerr_endline "Parsing error";
      exit 1