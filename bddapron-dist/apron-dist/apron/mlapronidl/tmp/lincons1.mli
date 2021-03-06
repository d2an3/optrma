(* File generated from lincons1.idl *)

type ap_lincons1_t = {
  mutable_lincons0: Lincons0.ap_lincons0_t;
  mutable_env: Environment.ap_environment_ptr;
}
and ap_lincons1_array_t = {
  mutable_lincons0_array: Lincons0.ap_lincons0_array_t;
  mutable_array_env: Environment.ap_environment_ptr;
}

(** APRON Constraints and array of constraints of level 1 *)

type typ = Lincons0.typ =
| EQ
| SUPEQ
| SUP
| DISEQ
| EQMOD of Scalar.t


(** Make a linear constraint. Modifying later the linear expression ({e not
advisable}) modifies correspondingly the linear constraint and conversely,
except for changes of environements *)
val make: Linexpr1.t -> typ -> t

(** Copy (deep copy) *)
val copy: t -> t

(** Convert a constraint type to a string ([=],[>=], or [>]) *)
val string_of_typ : typ -> string

(** Print the linear constraint *)
val print : Format.formatter -> t -> unit

(** Get the constraint type *)
val get_typ: t -> typ

(** Iter the function on the pair coefficient/variable of the underlying linear
expression *)
val iter: (Coeff.t -> Var.t -> unit) -> t -> unit

(** Get the constant of the underlying linear expression *)
val get_cst: t -> Coeff.t

(** Set the constraint type *)
val set_typ: t -> typ -> unit

(** Set simultaneously a number of coefficients.

[set_list expr [(c1,"x"); (c2,"y")] (Some cst)] assigns coefficients [c1] 
to variable ["x"], coefficient [c2] to variable ["y"], and coefficient [cst]
to the constant. If [(Some cst)] is replaced by [None],
the constant coefficient is not assigned. *)
val set_list : t -> (Coeff.t * Var.t) list -> Coeff.t option -> unit

(** Set simultaneously a number of coefficients, as [set_list]. *)
val set_array : t -> (Coeff.t * Var.t) array -> Coeff.t option -> unit

(** Set the constant of the underlying linear expression *)
val set_cst: t -> Coeff.t -> unit

(** Get the coefficient of the variable in the underlying linear expression *)
external ap_lincons1_get_coeff : ap_lincons1_t -> Var.ap_var_t -> Coeff.ap_coeff_t
	= "camlidl_lincons1_ap_lincons1_get_coeff"

(** Set the coefficient of the variable in the underlying linear expression *)
external ap_lincons1_set_coeff : ap_lincons1_t -> Var.ap_var_t -> Coeff.ap_coeff_t -> unit
	= "camlidl_lincons1_ap_lincons1_set_coeff"

(** Build the unsatisfiable constraint -1>=0 *)
external ap_lincons1_make_unsat : Environment.ap_environment_ptr -> ap_lincons1_t
	= "camlidl_lincons1_ap_lincons1_make_unsat"

(** Is the constraint not satisfiable ? *)
external ap_lincons1_is_unsat : ap_lincons1_t -> bool
	= "camlidl_lincons1_ap_lincons1_is_unsat"

(** Change the environement of the constraint for a super-environement. Raise [Failure] if it is not the case *)
external ap_lincons1_extend_environment : ap_lincons1_t -> Environment.ap_environment_ptr -> ap_lincons1_t
	= "camlidl_lincons1_ap_lincons1_extend_environment"

(** Side-effect version of the previous function *)
external ap_lincons1_extend_environment_with : ap_lincons1_t -> Environment.ap_environment_ptr -> unit
	= "camlidl_lincons1_ap_lincons1_extend_environment_with"


(** Get the environement of the linear constraint *)
val get_env: t -> Environment.t

(** Get the underlying linear expression. Modifying the linear expression ({e
not advisable}) modifies correspondingly the linear constraint and
conversely, except for changes of environements *)
val get_linexpr1: t -> Linexpr1.t

(** Get the underlying linear constraint of level 0. Modifying the constraint
of level 0 ({e not advisable}) modifies correspondingly the linear constraint
and conversely, except for changes of environements*)
val get_lincons0: t -> Lincons0.t


(* ====================================================================== *)
(** {2 Type array} *)
(* ====================================================================== *)

(** Make an array of linear constraints with the given size and defined on the
given environement. The elements are initialized with the constraint 0=0. *)
val array_make : Environment.t -> int -> earray

(** Print an array of constraints *)
val array_print :
?first:(unit, Format.formatter, unit) format ->
?sep:(unit, Format.formatter, unit) format ->
?last:(unit, Format.formatter, unit) format ->
Format.formatter -> earray -> unit

(** Get the size of the array *)
val array_length : earray -> int

(** Get the environment of the array *)
val array_get_env : earray -> Environment.t

(** Get the element of the given index (which is not a copy) *)
val array_get : earray -> int -> t

(** Set the element of the given index (without any copy). The array and the
constraint should be defined on the same environement; otherwise a [Failure]
exception is raised.*)
val array_set : earray -> int -> t -> unit

(** Change the environement of the array of constraints for a super-environement. Raise [Failure] if it is not the case*)
external ap_lincons1_array_extend_environment : ap_lincons1_array_t -> Environment.ap_environment_ptr -> ap_lincons1_array_t
	= "camlidl_lincons1_ap_lincons1_array_extend_environment"

(** Side-effect version of the previous function *)
external ap_lincons1_array_extend_environment_with : ap_lincons1_array_t -> Environment.ap_environment_ptr -> unit
	= "camlidl_lincons1_ap_lincons1_array_extend_environment_with"

