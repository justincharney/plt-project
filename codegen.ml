(* ocamlbuild -pkgs llvm codegen.native -no-hygiene *)
module L = Llvm
module StringMap = Map.Make(String)

let translate  = 
  let context = L.global_context () in
  let the_module = L.create_module context "P.A.T." in

  let i32_t = L.i32_type context 
  and i8_t = L.i8_type context 
  and i1_t = L.i1_type context in 

  let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in

  let formal_types = Array.of_list [] in
  let ftype = L.function_type i32_t formal_types in 
  let func_decl = L.define_function "main" ftype the_module in 

  let builder = L.builder_at_end context (L.entry_block func_decl) in 
  let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder in 

  let local = L.build_alloca i32_t "x" builder in 
  ignore(L.build_store (L.const_null i32_t) local builder);
  let load_local = L.build_load local "x" builder in 
  ignore(L.build_call printf_func [| int_format_str; load_local |] "printf" builder);

  let add_terminal builder instr = 
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore(instr builder) in 

  add_terminal builder (L.build_ret (L.const_int i32_t 0));  

  the_module(*creates llvm ir where number given value and printed*)

(*let translate  = 
  let context = L.global_context () in
  let the_module = L.create_module context "P.A.T." in

  let i32_t = L.i32_type context 
  and i8_t = L.i8_type context 
  and i1_t = L.i1_type context in 

  let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in

  let formal_types = Array.of_list [] in
  let ftype = L.function_type i32_t formal_types in 
  let func_decl = L.define_function "main" ftype the_module in 

  let builder = L.builder_at_end context (L.entry_block func_decl) in 
  let int_format_str = L.build_global_stringptr "%s\n" "fmt" builder in 
  let toprint = L.build_global_stringptr "Hello World" "mystr" builder in 

  ignore(L.build_call printf_func [| int_format_str; toprint|] "printf" builder);

  let add_terminal builder instr = 
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore(instr builder) in 

  add_terminal builder (L.build_ret (L.const_int i32_t 0));  

  the_module*)(*print string to console*)

(*let translate  = 
  let context = L.global_context () in
  let the_module = L.create_module context "P.A.T." in

  let i32_t = L.i32_type context 
  and i8_t = L.i8_type context 
  and i1_t = L.i1_type context 
  and i64_t = L.i64_type context in 

  let struct1 = L.named_struct_type context "MyString" in
  L.struct_set_body struct1 [| L.pointer_type i8_t; i64_t |] false;


  let printf_t : L.lltype = 
    L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
  let printf_func : L.llvalue = 
    L.declare_function "printf" printf_t the_module in

  let formal_types = Array.of_list [] in
  let ftype = L.function_type i32_t formal_types in 
  let func_decl = L.define_function "main" ftype the_module in 

  let builder = L.builder_at_end context (L.entry_block func_decl) in 
  let int_format_str = L.build_global_stringptr "%s\n" "fmt" builder in 
  let toprint = L.build_global_stringptr "Hello World" "fmt" builder in

  let local = L.build_alloca struct1 "x" builder in 
  let locX = L.build_struct_gep local 0 "y" builder in 
  let locx_new = L.build_store toprint locX builder in
  let locB = L.build_struct_gep local 0 "z" builder in
  let load_B = L.build_load locB "a" builder in
  let newa = L.build_call printf_func [| int_format_str; load_B |] "printf" builder in
  (*ignore(L.build_struct_gep local 0 "y" builder);
  let load_local = L.build_load local "y" builder in 
  ignore(L.build_call printf_func [| int_format_str; load_local |] "printf" builder);*)

  let add_terminal builder instr = 
    match L.block_terminator (L.insertion_block builder) with
      Some _ -> ()
    | None -> ignore(instr builder) in 

  add_terminal builder (L.build_ret (L.const_int i32_t 0));  

  the_module
*)
let () = 
  print_string (L.string_of_llmodule (translate))
