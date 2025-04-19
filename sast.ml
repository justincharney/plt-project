(* semant.ml *)
open Ast
module StringMap = Map.Make (String)

(* -------------------------------------------------------------------- *)
(* environments -------------------------------------------------------- *)
type ty = type_expr                          (* alias for brevity *)

type value_entry =
  | Var   of ty * bool                       (* type, is_const *)
  | Func  of func_decl
  | Struct of field list
  | TypeAlias of ty                          (* plain alias *)

type env = {
  types  : ty StringMap.t;                   (* names → named types *)
  values : value_entry StringMap.t;          (* vars / funcs / structs *)
}

let empty = { types = StringMap.empty; values = StringMap.empty }

let add_type name ty env =
  { env with types = StringMap.add name ty env.types }

let add_value name v env =
  { env with values = StringMap.add name v env.values }

let find_type name env = StringMap.find_opt name env.types
let find_value name env = StringMap.find_opt name env.values

(* -------------------------------------------------------------------- *)
exception SemError of string * Location.t option
let error ?loc msg = raise (SemError (msg,loc))

(* -------------------------------------------------------------------- *)
(* type equality helper (very light for now) *)
let rec same_type a b =
  match a,b with
  | Primitive p1, Primitive p2 -> p1 = p2
  | TypeName n1, TypeName n2   -> n1 = n2
  | Array (t1,l1), Array(t2,l2) -> l1 = l2 && same_type t1 t2
  | Slice t1, Slice t2         -> same_type t1 t2
  | Struct n1, Struct n2       -> n1 = n2
  | _ -> false

(* -------------------------------------------------------------------- *)
(* main inference function returns the type of an expression + an updated AST *)
let rec infer_expr env = function
  | Identifier x as e ->
      begin match find_value x env with
      | Some (Var (t,_)) -> (t, e)
      | Some (Func _)    -> error ("function used as value: "^x) None
      | Some (Struct _)  -> error ("struct value not allowed: "^x) None
      | None ->
          error ("unbound identifier "^x) None
      end

  | FunctionCall (name,[arg]) ->
      (* might be a cast *)
      begin match find_type name env with
      | Some ty ->
          let (targ,arg') = infer_expr env arg in
          (* simple rule: every primitive can cast to every primitive;
             fancier rules later *)
          if true then
            (ty, Cast (ty,arg'))
          else
            error "bad cast" None
      | None ->
          (* really a function call, look up its declaration *)
          begin match find_value name env with
          | Some (Func f) ->
              (* check arity, etc. *)
              let args_t, args' =
                List.split (List.map (infer_expr env) [arg]) in
              (* type‑check skipped … *)
              (List.hd f.return_types, FunctionCall(name,args'))
          | _ -> error ("unknown function "^name) None
          end
      end

  | FunctionCall (name,args) ->
      begin match find_value name env with
      | Some (Func f) ->
          let arg_ts, args' = List.split (List.map (infer_expr env) args) in
          if List.length arg_ts <> List.length f.params
          then error "arity mismatch" None;
          (* check each param type … *)
          (List.hd f.return_types, FunctionCall(name,args'))
      | _ -> error ("unknown function "^name) None
      end

  | Binop (e1,op,e2) ->
      let (t1,e1') = infer_expr env e1
      and (t2,e2') = infer_expr env e2 in
      if not (same_type t1 t2)
      then error "type mismatch in binary op" None;
      (t1, Binop(e1',op,e2'))

  | _ as other ->
      (* TODO: all other cases; return a dummy type for now *)
      (Primitive Bool, other)

(* -------------------------------------------------------------------- *)
let check_var_decl env (d: stmt) =
  match d with
  | VarDecl { name; var_type = None; initializer_expr = Some e; _ } ->
      let (t,e') = infer_expr env e in
      let entry   = Var (t,false) in
      let env'    = add_value name entry env in
      env', VarDecl { d with var_type = Some t; initializer_expr = Some e' }
  | VarDecl { name; var_type = Some t; _ } as vd ->
      let env' = add_value name (Var(t,false)) env in
      env', vd
  | _ -> env, d

(* -------------------------------------------------------------------- *)
let rec check_stmt env = function
  | Expr e ->
      let _t,e' = infer_expr env e in
      env, Expr e'

  | VarDecl _ as vd ->
      check_var_decl env vd

  | Block ss ->
      let env_local = ref env in
      let ss' = List.rev_map
        (fun s -> let (en',s') = check_stmt !env_local s in env_local := en'; s')
        ss
      in env, Block (List.rev ss')

  | IfStmt (cond,thn,else_opt) ->
      let _,cond' = infer_expr env cond in
      let _,thn'  = check_stmt env (Block thn) in
      let else_opt' =
        Option.map (fun s -> let _,s' = check_stmt env s in s') else_opt in
      env, IfStmt (cond',match thn' with Block b -> b | _ -> [thn'],
                   else_opt')

  | _ as s -> env, s        (* TODO: for/while/return … *)

(* -------------------------------------------------------------------- *)
let check_func env (f:func_decl) =
  let env_with_params =
    List.fold_left
      (fun e p -> add_value p.name (Var(p.param_type,true)) e)
      env f.params in
  let _,body' = check_stmt env_with_params (Block f.body) in
  { f with body = (match body' with Block b -> b | _ -> f.body) }

(* -------------------------------------------------------------------- *)
let analyse (prog:program) : program =
  (* 1. bootstrap global env with primitive types *)
  let env0 =
    List.fold_left
      (fun e p -> add_type p (Primitive p) e)
      empty
      [ Bool; String; U8; U16; U32; U64; I8; I16; I32; I64; F16; F32; Error ]
  in
  (* 2. add named types / structs from type_declarations *)
  let env_types =
    List.fold_left
      (fun env -> function
         | TypeStruct (name,_) as td ->
             add_type name (Struct name) env
         | TypeAlias (name,te) ->
             add_type name te env)
      env0 prog.type_declarations
  in
  (* 3. add function signatures *)
  let env_funcs =
    List.fold_left
      (fun e f -> add_value f.name (Func f) e)
      env_types prog.functions
  in
  (* 4. check globals (vars) *)
  let env_globals, globals' =
    List.fold_left
      (fun (e,acc) g ->
         let env',vd =
           check_var_decl e (VarDecl {
             is_const=g.is_const; name=g.name; var_type=g.var_type;
             initializer_expr=g.initializer_expr })
         in env', (match vd with VarDecl v -> { g with
                                               var_type=v.var_type;
                                               initializer_expr=v.initializer_expr }
                   | _ -> g) :: acc)
      (env_funcs,[]) prog.global_vars
  in
  (* 5. check each function body *)
  let funcs' =
    List.map (check_func env_globals) prog.functions
  in
  { prog with global_vars = List.rev globals';
              functions   = funcs' }
