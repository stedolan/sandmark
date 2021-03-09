(************************************************************************)
(*         *   The Coq Proof Assistant / The Coq Development Team       *)
(*  v      *         Copyright INRIA, CNRS and contributors             *)
(* <O___,, * (see version control and CREDITS file for authors & dates) *)
(*   \VV/  **************************************************************)
(*    //   *    This file is distributed under the terms of the         *)
(*         *     GNU Lesser General Public License Version 2.1          *)
(*         *     (see LICENSE file for the text of the license)         *)
(************************************************************************)

Require Import Ltac2.Init.

Ltac2 @ external type : constr -> constr := "ltac2" "constr_type".
(** Return the type of a term *)

Ltac2 @ external equal : constr -> constr -> bool := "ltac2" "constr_equal".
(** Strict syntactic equality: only up to α-conversion and evar expansion *)

Module Unsafe.

(** Low-level access to kernel terms. Use with care! *)

Ltac2 Type case.

Ltac2 Type kind := [
| Rel (int)
| Var (ident)
| Meta (meta)
| Evar (evar, constr array)
| Sort (sort)
| Cast (constr, cast, constr)
| Prod (binder, constr)
| Lambda (binder, constr)
| LetIn (binder, constr, constr)
| App (constr, constr array)
| Constant (constant, instance)
| Ind (inductive, instance)
| Constructor (constructor, instance)
| Case (case, constr, constr, constr array)
| Fix (int array, int, binder array, constr array)
| CoFix (int, binder array, constr array)
| Proj (projection, constr)
| Uint63 (uint63)
| Float (float)
].

Ltac2 @ external kind : constr -> kind := "ltac2" "constr_kind".

Ltac2 @ external make : kind -> constr := "ltac2" "constr_make".

Ltac2 @ external check : constr -> constr result := "ltac2" "constr_check".
(** Checks that a constr generated by unsafe means is indeed safe in the
    current environment, and returns it, or the error otherwise. Panics if
    not focused. *)

Ltac2 @ external substnl : constr list -> int -> constr -> constr := "ltac2" "constr_substnl".
(** [substnl [r₁;...;rₙ] k c] substitutes in parallel [Rel(k+1); ...; Rel(k+n)] with
    [r₁;...;rₙ] in [c]. *)

Ltac2 @ external closenl : ident list -> int -> constr -> constr := "ltac2" "constr_closenl".
(** [closenl [x₁;...;xₙ] k c] abstracts over variables [x₁;...;xₙ] and replaces them with
    [Rel(k); ...; Rel(k+n-1)] in [c]. If two names are identical, the one of least index is kept. *)

Ltac2 @ external case : inductive -> case := "ltac2" "constr_case".
(** Generate the case information for a given inductive type. *)

Ltac2 @ external constructor : inductive -> int -> constructor := "ltac2" "constr_constructor".
(** Generate the i-th constructor for a given inductive type. Indexing starts
    at 0. Panics if there is no such constructor. *)

End Unsafe.

Module Binder.

Ltac2 @ external make : ident option -> constr -> binder := "ltac2" "constr_binder_make".
(** Create a binder given the name and the type of the bound variable. *)

Ltac2 @ external name : binder -> ident option := "ltac2" "constr_binder_name".
(** Retrieve the name of a binder. *)

Ltac2 @ external type : binder -> constr := "ltac2" "constr_binder_type".
(** Retrieve the type of a binder. *)

End Binder.

Ltac2 @ external in_context : ident -> constr -> (unit -> unit) -> constr := "ltac2" "constr_in_context".
(** On a focused goal [Γ ⊢ A], [in_context id c tac] evaluates [tac] in a
    focused goal [Γ, id : c ⊢ ?X] and returns [fun (id : c) => t] where [t] is
    the proof built by the tactic. *)

Ltac2 @ external pretype : preterm -> constr := "ltac2" "constr_pretype".
(** Pretype the provided preterm. Assumes the goal to be focussed. *)