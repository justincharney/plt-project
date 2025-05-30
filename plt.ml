open Sast_printer

let () =
  let usage_msg = "Usage: " ^ Sys.argv.(0) ^ " <source_file.pat>" in
  let channel, filename =
    if Array.length Sys.argv = 2 then
      let filename = Sys.argv.(1) in
      try (open_in filename, filename)
      with Sys_error msg ->
        Printf.eprintf "Error: Cannot open file '%s': %s\n" filename msg;
        exit 1
    else (
      Printf.eprintf "%s\n" usage_msg;
      exit 1
    )
  in
  let lexbuf = Lexing.from_channel channel in
  (* Set the filename in lexbuf for better error messages *)
  lexbuf.lex_curr_p <- { lexbuf.lex_curr_p with pos_fname = filename };

  (* Reset ASI state before processing a new file *)
  Scanner.need_semi   := false;
  Scanner.paren_depth := 0;

  try
    (* 1. Parse the input file *)
    let ast = Parser.program Scanner.token lexbuf in

    (* 2. Perform semantic checks *)
    let sast = Semant.check_program ast in

    (* 3. Print the resulting SAST *)
    print_endline (string_of_sprogram sast);

    (* 4. Generate the LLVM IR and save to a .ll file *)
    let llvm_module = Irgen.translate sast in
    let base_output_path = Filename.remove_extension filename in
    let ll_filename = base_output_path ^ ".ll" in
    let s_filename = base_output_path ^ ".s" in
    let exe_filename = base_output_path ^ "_exe" in

    let ll_channel = open_out ll_filename in
    output_string ll_channel(Llvm.string_of_llmodule llvm_module);
    close_out ll_channel;

    Printf.printf"LLVM IR successfully generated and saved to: %s\n" ll_filename;
    Printf.printf"To compile and run: \n";
    Printf.printf"    llc-14 -relocation-model=pic %s -o %s\n" ll_filename s_filename;
    Printf.printf"    clang-14 -fPIE %s -o %s\n" s_filename exe_filename;
    Printf.printf"    %s\n" exe_filename;

    close_in channel;
    exit 0 (* Indicate success *)

  with
  | Scanner.Lexer_error msg ->
      let pos = lexbuf.lex_curr_p in
      Printf.eprintf "Lexer error at %s line %d character %d: %s\n"
        pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) msg;
      close_in channel;
      exit 1
  | Parsing.Parse_error ->
      let pos = lexbuf.lex_curr_p in
      let offending_token = Lexing.lexeme lexbuf in
      let base_msg =
        Printf.sprintf "Syntax error at %s line %d character %d near token '%s'\n"
        pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1) offending_token
      in
      let hint =
        match offending_token with
        | ":" -> ".\n Hint: Unexpected ':'. Did you mean ':=' for a short variable declaration?\n Hint: Or, if declaring a variable with a type, did you forget the 'const' keyword before the variable name?"
        | "=" -> ".\n Hint: Unexpected '='. For assignment use '='. For short form variable declaration and initialization, use ':='."
        | _ -> "."
      in
      Printf.printf "%s%s\n" base_msg hint;
      close_in channel;
      exit 1
   | Semant.Semantic_error msg ->
      (* Semantic errors might not have a specific token position easily available *)
      (* Depending on how Semant reports errors, you might add position info here *)
      Printf.eprintf "Semantic error: %s\n" msg;
      close_in channel;
      exit 1
   | Failure msg -> (* Catch other potential failures, e.g., from scanner helpers *)
       Printf.eprintf "Error: %s\n" msg;
       close_in channel;
       exit 1
   | ex -> (* Catch any other exceptions *)
        Printf.eprintf "An unexpected error occurred: %s\n" (Printexc.to_string ex);
        Printexc.print_backtrace stderr;
        close_in channel;
        exit 1
