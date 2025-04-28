open OUnit2
open Ast
open Semant (* The module we are testing *)

(* Helper to create a minimal valid program structure *)
let make_program ?(imports=[]) ?(types=[]) ?(globals=[]) ?(functions=[]) ?(struct_funcs=[]) () =
  {
    package_name = "main";
    imports = imports;
    type_declarations = types;
    global_vars = globals;
    functions = functions;
    struct_functions = struct_funcs;
  }

(* Helper function to run the semantic check and assert success *)
let run_semant_test_pass program _ =
  try
    let _ = Semant.check_program program in (* We only care that it doesn't raise *)
    assert_bool "Semantic check passed as expected." true
  with
  | Semantic_error msg -> assert_failure ("Semantic check failed unexpectedly: " ^ msg)
  | ex -> assert_failure ("An unexpected exception occurred: " ^ Printexc.to_string ex)

(* Helper function to run the semantic check and assert failure *)
let run_semant_test_fail program _ =
  assert_raises (Semantic_error "") (fun () -> Semant.check_program program)

(* --- Test Cases --- *)

let basic_tests = "Basic Valid Programs" >::: [
  "empty main" >:: run_semant_test_pass (make_program ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }] ());

  "global variable declaration (inferred)" >:: run_semant_test_pass (make_program
    ~globals:[{ is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 10) }]
    ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
    ()
  );

  "global variable declaration (explicit)" >:: run_semant_test_pass (make_program
    ~globals:[{ is_const=false; name="x"; var_type=Some (Primitive I32); initializer_expr=Some (IntLit 10) }]
    ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
    ()
  );

  "simple function call" >:: run_semant_test_pass (make_program
    ~functions:[
      { name="foo"; params=[]; return_types=[]; body=[] };
      { name="main"; params=[]; return_types=[]; body=[Expr (FunctionCall("foo", []))] }
    ]
    ()
  );

   "simple return" >:: run_semant_test_pass (make_program
    ~functions:[{ name="main"; params=[]; return_types=[Primitive I32]; body=[Return (Some [IntLit 42])] }]
    ()
  );
]

let type_tests = "Type Checking and Resolution" >::: [
    "valid assignment" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=Some (Primitive I32); initializer_expr=None};
            Expr (SimpleAssign(Identifier "x", IntLit 5))
        ]}]
        ()
    );

    "valid inferred assignment" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some (FloatLit 3.14)};
            Expr (SimpleAssign(Identifier "x", FloatLit 2.71))
        ]}]
        ()
    );

    "type alias usage" >:: run_semant_test_pass (make_program
        ~types:[TypeAlias("MyInt", Primitive I32)]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=Some (TypeName "MyInt"); initializer_expr=Some (IntLit 1)};
            Expr (SimpleAssign(Identifier "x", IntLit 2))
        ]}]
        ()
    );

    "array literal type check" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (ArrayLit (Primitive Bool, [BoolLit true; BoolLit false]))
        ]}]
        ()
    );

    "slice literal type check" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SliceLit (Primitive String, [StringLit "a"; StringLit "b"]))
        ]}]
        ()
    );

    "valid cast" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
             VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 10)};
             Expr (Cast(Primitive I64, Identifier "x"))
        ]}]
        ()
    );

    "null assignment to slice" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
             VarDecl {is_const=false; name="s"; var_type=Some(Slice(Primitive I32)); initializer_expr=None};
             Expr (SimpleAssign(Identifier "s", Null))
        ]}]
        ()
     );
]

let expression_tests = "Expression Checking" >::: [
    "valid binary operation (+)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(IntLit 1, Plus, IntLit 2))
        ]}]
        ()
    );
    "valid binary operation (==)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(FloatLit 1.0, Eq, FloatLit 2.0))
        ]}]
        ()
    );
    "valid binary operation (&&)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(BoolLit true, And, BoolLit false))
        ]}]
        ()
    );
    "valid unary operation (-)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Unaop(Neg, IntLit 5))
        ]}]
        ()
    );
    "valid unary operation (!)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Unaop(Not, BoolLit true))
        ]}]
        ()
    );
    "valid compound assignment" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 10)};
            Expr (CompoundAssign(Identifier "x", PlusAssign, IntLit 5))
        ]}]
        ()
    );
    "valid sequence" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Sequence(IntLit 1, IntLit 2))
        ]}]
        ()
    );
    "valid index access" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="a"; var_type=None; initializer_expr=Some(ArrayLit(Primitive I32, [IntLit 1]))};
            Expr (IndexAccess(Identifier "a", IntLit 0))
        ]}]
        ()
    );
     "valid slice expression" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="a"; var_type=None; initializer_expr=Some(ArrayLit(Primitive I32, [IntLit 1; IntLit 2]))};
            Expr (SliceExpr(Identifier "a", Some (IntLit 0), Some (IntLit 1)))
        ]}]
        ()
    );
    "valid make" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr(Make(Slice(Primitive U8), IntLit 10, Some (IntLit 20)))
        ]}]
        ()
    );
]

let statement_tests = "Statement Checking" >::: [
    "valid if statement" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            IfStmt(BoolLit true, [Expr(IntLit 1)], Some (Block [Expr(IntLit 2)]))
        ]}]
        ()
    );
    "valid while statement" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            WhileStmt(BoolLit false, [Expr(IntLit 1)])
        ]}]
        ()
    );
    "valid for statement (full)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            ForStmt(
                Some (VarDecl {is_const=false; name="i"; var_type=None; initializer_expr=Some(IntLit 0)}),
                Some (Binop(Identifier "i", Lt, IntLit 10)),
                Some (CompoundAssign(Identifier "i", PlusAssign, IntLit 1)),
                [Expr(Identifier "i")]
            )
        ]}]
        ()
    );
    "valid for statement (condition only)" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            ForStmt( None, Some (BoolLit true), None, [Break] )
        ]}]
        ()
    );
    "valid return (multiple values)" >:: run_semant_test_pass (make_program
        ~functions:[{
            name="swap";
            params=[{name="a"; param_type=Primitive I32}; {name="b"; param_type=Primitive I32}];
            return_types=[Primitive I32; Primitive I32];
            body=[Return(Some [Identifier "b"; Identifier "a"])]
        }; { name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
     "valid block scope" >:: run_semant_test_pass (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Block [
                VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)}
            ];
             (* x is not visible here *)
            VarDecl {is_const=false; name="y"; var_type=None; initializer_expr=Some(IntLit 2)}
        ]}]
        ()
    );
]

let struct_method_tests = "Structs and Methods" >::: [
    "define simple struct" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
    "struct literal creation" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Point", [
                {name="x"; field_type=Primitive I32; modifier=None; default_value=None};
                {name="y"; field_type=Primitive I32; modifier=None; default_value=None}
            ])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (StructLit("Point", [("x", IntLit 1); ("y", IntLit 2)]))
        ]}]
        ()
    );
    "struct field access" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="p"; var_type=None; initializer_expr=Some(StructLit("Point", [("x", IntLit 1)]))};
            Expr (FieldAccess(Identifier "p", "x"))
        ]}]
        ()
    );
    "simple method definition" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Counter", [{name="val"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~struct_funcs:[{
            name="get"; struct_name="Counter"; params=[]; return_types=[Primitive I32];
            body=[Return(Some [FieldAccess(Identifier "self", "val")])] (* Assuming self convention *)
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
    "simple method call" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Counter", [{name="val"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~struct_funcs:[{
            name="get"; struct_name="Counter"; params=[]; return_types=[Primitive I32];
            body=[Return(Some [FieldAccess(Identifier "self", "val")])]
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="c"; var_type=None; initializer_expr=Some(StructLit("Counter", [("val", IntLit 5)]))};
            Expr (MethodCall(Identifier "c", "get", []))
        ]}]
        ()
    );
     "method with params" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Adder", [])]
        ~struct_funcs:[{
            name="add"; struct_name="Adder";
            params=[{name="a"; param_type=Primitive I32}; {name="b"; param_type=Primitive I32}];
            return_types=[Primitive I32];
            body=[Return(Some [Binop(Identifier "a", Plus, Identifier "b")])]
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
             VarDecl {is_const=false; name="ad"; var_type=None; initializer_expr=Some(StructLit("Adder", []))};
            Expr (MethodCall(Identifier "ad", "add", [IntLit 3; IntLit 4]))
        ]}]
        ()
    );
     "null assignment to struct" >:: run_semant_test_pass (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
             VarDecl {is_const=false; name="p"; var_type=Some(Struct("Point")); initializer_expr=None};
             Expr (SimpleAssign(Identifier "p", Null))
        ]}]
        ()
     );
]

(* --- Failure Cases --- *)

let error_tests_declarations = "Error Cases: Declarations" >::: [
    "duplicate global var" >:: run_semant_test_fail (make_program
        ~globals:[
            {is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 1)};
            {is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 2)}
        ]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
    "duplicate function" >:: run_semant_test_fail (make_program
        ~functions:[
            { name="foo"; params=[]; return_types=[]; body=[] };
            { name="foo"; params=[]; return_types=[]; body=[] };
            { name="main"; params=[]; return_types=[]; body=[] }
        ]
        ()
    );
    "duplicate type alias" >:: run_semant_test_fail (make_program
        ~types:[TypeAlias("MyInt", Primitive I32); TypeAlias("MyInt", Primitive U32)]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
    "duplicate struct" >:: run_semant_test_fail (make_program
        ~types:[
            TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}]);
            TypeStruct("Point", [{name="y"; field_type=Primitive F32; modifier=None; default_value=None}])
        ]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
    "duplicate method" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Thing", [])]
        ~struct_funcs:[
            {name="doIt"; struct_name="Thing"; params=[]; return_types=[]; body=[]};
            {name="doIt"; struct_name="Thing"; params=[]; return_types=[]; body=[]}
        ]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
     "variable declaration no type no init" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=None}
        ]}]
        ()
    );
     "duplicate var in scope" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)};
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 2)}
        ]}]
        ()
    );
]

let error_tests_types = "Error Cases: Types" >::: [
    "assignment type mismatch" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=Some (Primitive I32); initializer_expr=None};
            Expr (SimpleAssign(Identifier "x", BoolLit true))
        ]}]
        ()
    );
    "declaration init type mismatch" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=Some (Primitive String); initializer_expr=Some(IntLit 10)}
        ]}]
        ()
    );
    "binop type mismatch (+)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(IntLit 1, Plus, BoolLit true))
        ]}]
        ()
    );
     "binop type mismatch (==)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(IntLit 1, Eq, StringLit "a"))
        ]}]
        ()
    );
     "binop type mismatch (&&)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Binop(IntLit 1, And, BoolLit true))
        ]}]
        ()
    );
    "unary type mismatch (-)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Unaop(Neg, BoolLit true))
        ]}]
        ()
    );
     "unary type mismatch (!)" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (Unaop(Not, IntLit 0))
        ]}]
        ()
    );
    "if condition not bool" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            IfStmt(IntLit 1, [], None)
        ]}]
        ()
    );
    "while condition not bool" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            WhileStmt(IntLit 0, [])
        ]}]
        ()
    );
    "for condition not bool" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            ForStmt(None, Some (IntLit 1), None, [])
        ]}]
        ()
    );
    "return type mismatch" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[Primitive I32]; body=[
            Return(Some [BoolLit true])
        ]}]
        ()
    );
    "return wrong number of values" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[Primitive I32]; body=[
            Return(Some [IntLit 1; IntLit 2])
        ]}]
        ()
    );
     "return value from void func" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Return(Some [IntLit 1])
        ]}]
        ()
    );
     "return nothing from non-void func" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[Primitive Bool]; body=[
            Return(None)
        ]}]
        ()
    );
     "array literal type mismatch" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (ArrayLit(Primitive I32, [IntLit 1; BoolLit true]))
        ]}]
        ()
    );
     "slice literal type mismatch" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SliceLit(Primitive F32, [FloatLit 1.0; StringLit "no"]))
        ]}]
        ()
    );
     "invalid cast" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
             Expr (Cast(Primitive Bool, IntLit 1))
        ]}]
        ()
    );
    "null assignment to non-nullable" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
             VarDecl {is_const=false; name="x"; var_type=Some(Primitive I32); initializer_expr=None};
             Expr (SimpleAssign(Identifier "x", Null))
        ]}]
        ()
     );
]

let error_tests_scope = "Error Cases: Scope and Usage" >::: [
    "use undeclared variable" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SimpleAssign(Identifier "x", IntLit 1))
        ]}]
        ()
    );
    "call undeclared function" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (FunctionCall("no_such_func", []))
        ]}]
        ()
    );
    "call variable as function" >:: run_semant_test_fail (make_program
        ~globals:[{is_const=false; name="x"; var_type=None; initializer_expr=Some (IntLit 1)}]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (FunctionCall("x", []))
        ]}]
        ()
    );
    "function wrong number of args" >:: run_semant_test_fail (make_program
        ~functions:[
            { name="foo"; params=[{name="a"; param_type=Primitive I32}]; return_types=[]; body=[] };
            { name="main"; params=[]; return_types=[]; body=[Expr (FunctionCall("foo", []))] }
        ]
        ()
    );
     "function wrong arg type" >:: run_semant_test_fail (make_program
        ~functions:[
            { name="foo"; params=[{name="a"; param_type=Primitive I32}]; return_types=[]; body=[] };
            { name="main"; params=[]; return_types=[]; body=[Expr (FunctionCall("foo", [BoolLit true]))] }
        ]
        ()
    );
     "assign to non-lvalue" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (SimpleAssign(IntLit 5, IntLit 10))
        ]}]
        ()
    );
     "index non-array/slice" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)};
            Expr (IndexAccess(Identifier "x", IntLit 0))
        ]}]
        ()
    );
     "index with non-integer" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="a"; var_type=None; initializer_expr=Some(ArrayLit(Primitive I32, [IntLit 1]))};
            Expr (IndexAccess(Identifier "a", BoolLit true))
        ]}]
        ()
    );
      "slice non-array/slice" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)};
            Expr (SliceExpr(Identifier "x", Some(IntLit 0), None))
        ]}]
        ()
    );
      "slice with non-integer" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="a"; var_type=None; initializer_expr=Some(ArrayLit(Primitive I32, [IntLit 1]))};
            Expr (SliceExpr(Identifier "a", Some(FloatLit 0.0), None))
        ]}]
        ()
    );
     "make len non-integer" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr(Make(Slice(Primitive U8), StringLit "10", None))
        ]}]
        ()
    );
     "make cap non-integer" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr(Make(Slice(Primitive U8), IntLit 10, Some(BoolLit true)))
        ]}]
        ()
    );
    "use undeclared type alias" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=Some(TypeName "NoSuchType"); initializer_expr=None}
        ]}]
        ()
    );
    "use undeclared struct type" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="p"; var_type=Some(Struct "NoSuchStruct"); initializer_expr=None}
        ]}]
        ()
    );
]

let error_tests_structs = "Error Cases: Structs and Methods" >::: [
    "struct literal unknown struct" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (StructLit("NoSuchStruct", []))
        ]}]
        ()
    );
    "struct literal unknown field" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (StructLit("Point", [("y", IntLit 1)]))
        ]}]
        ()
    );
    "struct literal field type mismatch" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            Expr (StructLit("Point", [("x", BoolLit true)]))
        ]}]
        ()
    );
    "field access on non-struct" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)};
            Expr (FieldAccess(Identifier "x", "some_field"))
        ]}]
        ()
    );
    "field access unknown field" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Point", [{name="x"; field_type=Primitive I32; modifier=None; default_value=None}])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="p"; var_type=None; initializer_expr=Some(StructLit("Point", [("x", IntLit 1)]))};
            Expr (FieldAccess(Identifier "p", "y"))
        ]}]
        ()
    );
     "method call on non-struct" >:: run_semant_test_fail (make_program
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="x"; var_type=None; initializer_expr=Some(IntLit 1)};
            Expr (MethodCall(Identifier "x", "doThing", []))
        ]}]
        ()
    );
     "method call unknown method" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Thing", [])]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="t"; var_type=None; initializer_expr=Some(StructLit("Thing", []))};
            Expr (MethodCall(Identifier "t", "noSuchMethod", []))
        ]}]
        ()
    );
    "method call wrong number of args" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Thing", [])]
         ~struct_funcs:[{
            name="doIt"; struct_name="Thing"; params=[{name="a"; param_type=Primitive I32}]; return_types=[];
            body=[]
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="t"; var_type=None; initializer_expr=Some(StructLit("Thing", []))};
            Expr (MethodCall(Identifier "t", "doIt", [])) (* Missing arg 'a' *)
        ]}]
        ()
    );
     "method call wrong arg type" >:: run_semant_test_fail (make_program
        ~types:[TypeStruct("Thing", [])]
         ~struct_funcs:[{
            name="doIt"; struct_name="Thing"; params=[{name="a"; param_type=Primitive I32}]; return_types=[];
            body=[]
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[
            VarDecl {is_const=false; name="t"; var_type=None; initializer_expr=Some(StructLit("Thing", []))};
            Expr (MethodCall(Identifier "t", "doIt", [BoolLit true])) (* Wrong type for 'a' *)
        ]}]
        ()
    );
     "method definition for unknown struct" >:: run_semant_test_fail (make_program
         ~struct_funcs:[{
            name="doIt"; struct_name="NoSuchStruct"; params=[]; return_types=[];
            body=[]
        }]
        ~functions:[{ name="main"; params=[]; return_types=[]; body=[] }]
        ()
    );
]


(* Add more test categories as needed: Control Flow, Arrays/Slices, etc. *)

let suite = "Semantic Analyzer Test Suite" >::: [
    basic_tests;
    type_tests;
    expression_tests;
    statement_tests;
    struct_method_tests;
    error_tests_declarations;
    error_tests_types;
    error_tests_scope;
    error_tests_structs;
  ]

let () =
  run_test_tt_main suite
