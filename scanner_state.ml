(* Hashtable to store known type names (mapping string name -> unit) *)
let known_types : (string, unit) Hashtbl.t = Hashtbl.create 16

(* Function called by the parser to register a new type name *)
let register_type_name (name : string) : unit =
  if not (Hashtbl.mem known_types name) then
    Hashtbl.add known_types name ()
  (* Optionally add a warning or error if redefining a type *)

(* Function called by the lexer to check if an identifier is a known type *)
let is_type_name (name : string) : bool =
  Hashtbl.mem known_types name

(* Pre-populate with built-in primitive type names *)
let () =
  List.iter register_type_name [
    "bool"; "string";
    "u8"; "u16"; "u32"; "u64";
    "i8"; "i16"; "i32"; "i64";
    "f16"; "f32";
    "error" (* Assuming 'error' acts like a type name *)
    (* Note: Keywords like BOOL, STRING, U8 are handled separately by the lexer/parser *)
    (* Adding them here makes `is_type_name` consistent if needed, but isn't strictly *)
    (* required for the parser rules as written *)
  ]
