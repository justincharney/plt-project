open OUnit2
open Ast
open Semant

(* ------------------------------------------------------------------ *)
(* Helpers to build tiny AST fragments quickly                         *)
(* ------------------------------------------------------------------ *)
let ident  x        = Identifier x
let intlit n        = IntLit n
let boollit b       = BoolLit b
let nillit          = Null
let var ?ty ?init n = {
  is_const = false; name = n; var_type = ty; initializer_expr = init
}

let fn ?(ret=[]) name params body =
  { name;
    params;
    return_types = ret;
    body
  }

let vdecl ?(is_const=false) name var_type initializer_expr =
  VarDecl { is_const; name; var_type; initializer_expr }

let param n ty = { name = n; param_type = ty }

let struct_type name fields = TypeStruct (name, fields)

let alias name te           = TypeAlias  (name, te)

let empty_prog =
  { package_name      = "Dummy"
  ; imports           = []
  ; type_declarations = []
  ; global_vars       = []
  ; functions         = []
  ; struct_functions  = []
  }

let run_ok p _ctx   = ignore (check_program p)
let run_err ?(msg="") p _ctx =
  assert_raises
    (Semantic_error msg)
    (fun () -> ignore (check_program p))

(* ------------------------------------------------------------------ *)
(*  Test cases                                                         *)
(* ------------------------------------------------------------------ *)

let t_return_ok =
  "return type ok" >:: run_ok
    { empty_prog with
      functions = [
        fn "f" [] ~ret:[ Primitive U32 ]
          [ Return (Some [ intlit 1 ]) ]
      ] }

let t_return_mismatch =
  "return type mismatch" >:: run_err
    { empty_prog with
      functions = [
        fn "f" [] ~ret:[ Primitive U32 ]
          [ Return (Some [ boollit true ]) ]
      ] }

let t_return_arity =
  "return arity mismatch" >:: run_err
    { empty_prog with
      functions = [
        fn "f" [] ~ret:[ Primitive U32 ; Primitive Bool ]
          [ Return (Some [ intlit 1 ]) ]
      ] }

let t_void_return_has_value =
  "void fn returns value" >:: run_err
    { empty_prog with
      functions = [
        fn "g" [] ~ret:[]
          [ Return (Some [ intlit 1 ]) ]
      ] }

let t_null_assign_slice =
  "null assign to slice" >:: run_ok
    { empty_prog with
      type_declarations = [ struct_type "S" [] ];
      functions = [
        fn "f" [] ~ret:[]
          [ vdecl "a" (Some (Slice (Primitive U32))) (Some nillit) ]
      ]}

let t_null_compare =
  "null == slice" >:: run_ok
    { empty_prog with
      functions = [
        fn "f" [] ~ret:[ Primitive Bool ]
          [ Return (Some [ Binop (nillit, Eq, ident "x") ]) ]
      ];
      global_vars = [ var ~ty:(Slice (Primitive U32))    "x" ]
    }

let t_lvalue_good =
  "identifier assignment ok" >:: run_ok
    { empty_prog with
      functions = [
        fn "f" [] ~ret:[]
          [ vdecl "x" (Some (Primitive U32)) (Some (intlit 0));
            Expr (SimpleAssign (ident "x", intlit 42)) ]
      ] }

let t_lvalue_bad =
  "literal assignment bad" >:: run_err
    { empty_prog with
      functions = [
        fn "f" [] ~ret:[]
          [ Expr (SimpleAssign (intlit 10, intlit 11)) ]
      ] }

let t_forward_ref_struct =
  "struct forward ref field" >:: run_ok
    { empty_prog with
      type_declarations = [
        struct_type "B" [ { name = "x"; field_type = TypeName "A"; modifier=None; default_value=None } ];
        struct_type "A" []
      ]
    }

let t_alias_forward =
  "alias forward to struct" >:: run_ok
    { empty_prog with
      type_declarations = [
        alias "T" (Struct "Later");
        struct_type "Later" []
      ];
      functions = [
        fn "f" [] ~ret:[ TypeName "T" ] [ Return (Some [ StructLit ("Later",[]) ]) ]
      ]
    }

let t_comp_assign_ok =
  "compound assign ok" >:: run_ok
    { empty_prog with
      functions = [
        fn "f" [ param "x" (Primitive U32) ] ~ret:[]
          [ Expr (CompoundAssign (ident "x", PlusAssign, intlit 1)) ]
      ]
    }

let suite =
  "semant" >::: [
    t_return_ok;
    t_return_mismatch;
    t_return_arity;
    t_void_return_has_value;
    t_null_assign_slice;
    t_null_compare;
    t_lvalue_good;
    t_lvalue_bad;
    t_forward_ref_struct;
    t_alias_forward;
    t_comp_assign_ok;
  ]

let () = run_test_tt_main suite
