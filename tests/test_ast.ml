open OUnit2
open Ast

let ast_construction_tests = "test suite for AST construction" >::: [

  "create literals" >:: (fun _ ->
    let _ = IntLit 10 in
    let _ = FloatLit 3.14 in
    let _ = BoolLit true in
    let _ = StringLit "hello" in
    let _ = CharLit 'a' in
    let _ = Null in
    assert_bool "Literal creation possible" true
  );

  "create type expressions" >:: (fun _ ->
    let _ = Primitive Bool in
    let _ = Pointer (Primitive U64) in
    let _ = Array (Primitive String, 10) in
    let _ = Slice (Pointer (Primitive F32)) in
    let _ = Struct "MyStruct" in
    let _ = TypeName "MyIntAlias" in
    assert_bool "Type expression creation possible" true
  );

  "create basic expressions" >:: (fun _ ->
    let _ = Identifier "x" in
    let _ = Binop (IntLit 1, Plus, IntLit 2) in
    let _ = Unaop (Neg, Identifier "y") in
    let _ = Assignment (Identifier "x", RegAssign, IntLit 5) in
    let _ = Assignment (Identifier "count", PlusAssign, IntLit 1) in
    let _ = FieldAccess (Identifier "myStructVar", Identifier "fieldName") in
    let _ = IndexAccess (Identifier "myArray", IntLit 0) in
    let _ = SliceExpr (Identifier "mySlice", IntLit 1, Some (IntLit 5)) in
    let _ = FunctionCall (Identifier "myFunc", [IntLit 1; BoolLit false]) in
    let _ = Cast (Primitive I16, Identifier "long_val") in
    let _ = Make (Slice (Primitive I32), IntLit 10, None) in
    let _ = ErrorExp (StringLit "Something went wrong") in
    let _ = Sequence (Assignment (Identifier "a", RegAssign, IntLit 1), Identifier "a") in
    assert_bool "Basic expression creation possible" true
  );

  "create statements" >:: (fun _ ->
    let _ = Expr (FunctionCall ("doSideEffect", [])) in
    let _ = Block [Expr (IntLit 1); Expr (IntLit 2)] in
    let _ = IfStmt (BoolLit true, [Expr (IntLit 1)], Some (Block [Expr (IntLit 0)])) in
    let _ = ForStmt (None, Some (BoolLit true), None, [Expr (IntLit 1)]) in
    let _ = WhileStmt (Identifier "running", [Expr (IntLit 1)]) in
    let _ = Return (Some [Identifier "result"]) in
    let _ = Return None in (* return; *)
    let _ = Break in
    let _ = Continue in
    let _ = VarDecl { is_const=false; name="x"; var_type=Some (Primitive I32); initializer_expr=Some (IntLit 10)} in (* var x i32 = 10 *)
    let _ = VarDecl { is_const=false; name="y"; var_type=None; initializer_expr=Some (FloatLit 1.0)} in (* y := 1.0 *)
    let _ = VarDecl { is_const=true; name="MAX_VAL"; var_type=Some (Primitive U64); initializer_expr=Some (IntLit 100)} in (* const MAX_VAL u64 = 100 *)
    let _ = VarDecl { is_const=false; name="z"; var_type=Some (Primitive Bool); initializer_expr=None} in (* var z bool *)

    assert_bool "Statement creation possible" true
  );

  "create top-level declarations" >:: (fun _ ->
    let simple_field = { name="id"; field_type=Primitive I32; modifier=None; default_value=None } in

    (* Test type_decl constructors *)
    let struct_decl: type_decl = TypeStruct ("MyStruct", [simple_field]) in
    let alias_decl: type_decl = TypeAlias ("UserID", Primitive I32) in

    (* Test func_decl record *)
    let simple_param = { name="input"; param_type=Primitive String } in
    let function_decl: func_decl = {
      name="process";
      params=[simple_param];
      return_types=[Primitive Bool];
      body=[Return (Some [BoolLit true])]
    } in

    (* Test global_decl record *)
    let global_var: global_decl = {
      is_const=true;
      name="PI";
      var_type=Some (Primitive F32);
      initializer_expr=Some (FloatLit 3.14159)
    } in

    assert_equal "MyStruct" (match struct_decl with TypeStruct(name, _) -> name | _ -> "");
    assert_equal "UserID" (match alias_decl with TypeAlias(name, _) -> name | _ -> "");
    assert_equal "process" function_decl.name;
    assert_equal true global_var.is_const;
    assert_equal "PI" global_var.name;
  );

  "create program structure" >:: (fun _ ->
    let program_struct: program = {
      package_name = "main";
      imports = ["fmt"; "math"];
      type_declarations = [TypeAlias ("Count", Primitive I32)];
      global_vars = [{
        is_const=true;
        name="Version";
        var_type=Some(Primitive String);
        initializer_expr=Some(StringLit "1.0")
      }];
      functions = [{
        name="main";
        params=[];
        return_types=[];
        body=[]
      }];
    } in

    assert_equal "main" program_struct.package_name;
    assert_equal 2 (List.length program_struct.imports);
    assert_equal 1 (List.length program_struct.type_declarations);
    assert_equal 1 (List.length program_struct.global_vars);
    assert_equal 1 (List.length program_struct.functions);
  )

]

(* Run test *)
let () = run_test_tt_main ast_construction_tests
