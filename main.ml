(* main.ml *)

open Lexing
open Parser
open Semantic

let () =
  Printexc.record_backtrace true;
  let lexbuf = from_channel stdin in
  try
    let ast  = program Scanner.token lexbuf in
    let sast = analyze ast in
    Pat_sast.pp_program sast;
    exit 0

  with
  | SemanticError msg ->
      prerr_endline ("Semantic error: " ^ msg);
      exit 1

  | Failure msg ->
      prerr_endline ("Lexing error: " ^ msg);
      exit 1

  | _ ->
      let pos  = lexbuf.lex_curr_p in
      let line = pos.pos_lnum in
      let col  = pos.pos_cnum - pos.pos_bol in
      let tok  = Lexing.lexeme lexbuf in
      prerr_endline (
        Printf.sprintf
          "Parse error at line %d, column %d: unexpected token %S"
          line col tok
      );
      exit 1
