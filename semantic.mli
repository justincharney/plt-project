(** semantic.mli *)

(** Raised on any semantic‐analysis failure. *)
exception SemanticError of string

(** The final “semantic AST” type — opaque to clients. *)
type sprogram

(** [analyze prog] builds the semantic AST or raises [SemanticError]. *)
val analyze       : Ast.program -> sprogram

(** [check_program prog] runs the checker for side‑effects
    (raises [SemanticError] on error, returns [()] on success). *)
val check_program : Ast.program -> unit
