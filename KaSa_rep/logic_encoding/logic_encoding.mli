(**
  * main.ml
  * openkappa
  * Yvan SRAKA, project Antique, INRIA Paris
  *
  * Creation: 2019, the 1th of Avril (not a joke)
  * Last modification: Time-stamp: <Apr 1 2019>
  *
  * Kappa site-graph patterns equations resolution
  *
  * Copyright 2010,2011,2012,2013,2014,2015,2016,2017, 2018, 2019
  * Institut National de Recherche en Informatique et en Automatique.
  * All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)


(** Summary
 **********
 *
 * This module expose :
 *
 *  - custom modules for logic formulas handling (made from MVBDU)
 *  - basic operation on logic formulas: OR / AND / etc...
 *
 * ... and will be completed soon with:
 *
 *  - utils to convert ckappa, cckappa, lkappa structures to the logic encoding
 *  - functions to create / update the structure incrementally
 *)


(** Basic types to handle site-graphs structure
  *********************************************)

type c_type = int
type c_state = int
type c_site_name = int
type c_binding = Bounded of c_site_name | Free | Unknown
type c_site = c_binding
type c_path = (c_site * c_site) list

type c_hyp = int
type c_refinement = int


(** Custom functor made from MVBDU
  ********************************)

module Formula_bdu : sig
  type key = c_hyp
  type value = c_refinement
  type mvbdu = Kappa_mvbdu.Mvbdu_wrapper.Mvbdu.mvbdu
end

(** internalized version *)

module Formula_intbdu : sig
  type key = c_hyp
  type value = c_refinement
  type mvbdu = Formula_bdu.mvbdu
end

(** generalized version *)

module Formula_G : sig
  type key = c_hyp (* TODO: 'a *)
  type value = c_refinement (* TODO: 'b *)
  type mvbdu = Formula_bdu.mvbdu
end


(** Few functions to combine logic sub-formulas into formulas
  ***********************************************************)

(** boolean encoding & semantic comparaison operator *)

val m_bool : bool -> Formula_G.mvbdu
val m_equal : 'a -> 'a -> Formula_G.mvbdu

(** binary operators *)

val m_imply : Formula_G.mvbdu -> Formula_G.mvbdu -> Formula_G.mvbdu
val m_equiv : Formula_G.mvbdu -> Formula_G.mvbdu -> Formula_G.mvbdu
val m_and : Formula_G.mvbdu -> Formula_G.mvbdu -> Formula_G.mvbdu
val m_or : Formula_G.mvbdu -> Formula_G.mvbdu -> Formula_G.mvbdu

(** quantifiers *)

val m_forall : 'a list -> ('a -> Formula_G.mvbdu) -> Formula_G.mvbdu
val m_exist : 'a list -> ('a -> Formula_G.mvbdu) -> Formula_G.mvbdu

(** submodule containing operators used in this module for writing in infix form
  * (I also think it's more intuitive/concise to use it that way) *)

module Logical_operators : sig
  val ( === ) : 'a -> 'a -> Formula_G.mvbdu
  val (  => ) : Formula_G.mvbdu -> Formula_G.mvbdu -> Formula_G.mvbdu
  val ( <=> ) : Formula_G.mvbdu -> Formula_G.mvbdu -> Formula_G.mvbdu
end


(** Utils to convert ckappa, cckappa, lkappa structures to the logic encoding
  ***************************************************************************)

val ckappa_to_mvbdu : Kappa_kasa_frontend.Ckappa_sig.kappa_handler -> Formula_G.mvbdu
val lkappa_to_mvbdu : 'a -> Formula_G.mvbdu (* TODO *)

val cckappa_to_mvbdu : Remanent_parameters_sig.parameters -> Exception_without_parameter.method_handler -> Kappa_kasa_frontend.Cckappa_sig.kappa_handler -> Formula_G.mvbdu

(** `g_static_support` is the intermediate representation passed to following axioms to build MVBDU *)

type g_static_support =
  { g_paths: c_path list
  ; g_aliases: c_path -> c_path list
  ; g_sites: c_type -> c_site list
  ; g_state: c_path -> c_site -> c_state
  ; g_states: c_type -> c_site -> c_state list
  ; g_type: c_path -> c_type
  ; g_types: c_type list }

(** Predicates *)

val p_agent : c_path -> Formula_G.mvbdu
val p_alias : c_path -> c_path -> Formula_G.mvbdu
val p_site  : c_path -> c_site -> c_site -> Formula_G.mvbdu
val p_type  : c_path -> c_type -> Formula_G.mvbdu

val p_agent_eval : g_static_support -> c_path -> Formula_G.mvbdu
val p_alias_eval : g_static_support -> c_path -> c_path -> Formula_G.mvbdu
val p_site_eval  : g_static_support -> c_path -> c_site -> c_site -> Formula_G.mvbdu
val p_type_eval  : g_static_support -> c_path -> c_type -> Formula_G.mvbdu

(** Axioms *)

val axiom_1 : g_static_support -> Formula_G.mvbdu
val axiom_2 : g_static_support -> Formula_G.mvbdu
val axiom_3 : g_static_support -> Formula_G.mvbdu
val axiom_4 : g_static_support -> Formula_G.mvbdu
val axiom_5 : g_static_support -> Formula_G.mvbdu
val axiom_6 : g_static_support -> Formula_G.mvbdu
val axiom_7 : g_static_support -> Formula_G.mvbdu
(* TODO *)
val axiom_9  : g_static_support -> Formula_G.mvbdu
val axiom_10 : g_static_support -> Formula_G.mvbdu
val axiom_11 : g_static_support -> Formula_G.mvbdu
val axiom_12 : g_static_support -> Formula_G.mvbdu
val axiom_13 : g_static_support -> Formula_G.mvbdu
(* TODO *)
val axiom_15 : g_static_support -> Formula_G.mvbdu
val axiom_16 : g_static_support -> Formula_G.mvbdu
val axiom_17 : g_static_support -> Formula_G.mvbdu
val axiom_18 : g_static_support -> Formula_G.mvbdu
val axiom_19 : g_static_support -> Formula_G.mvbdu

val axioms : g_static_support -> Formula_G.mvbdu
(** `axioms` apply all axioms to a context to generate a MVBDU *)

val main : Cckappa_sig.kappa_handler option -> unit
