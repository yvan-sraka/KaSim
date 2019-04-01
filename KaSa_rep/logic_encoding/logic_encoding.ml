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


(***************************************************)
(** Kappa site-graph patterns equations resolution *)
(***************************************************)

(** Abstract
  * ========
  *
  * Site-graphs rewriting is used to represent mechanistic models of
  * interactions between proteins, such as intra or extra cellular signaling
  * pathways, epigenetic information repair models, the self-assembly of
  * macro-molecules, etc.
  *
  * Many qualitative questions about these models come down to solving
  * algebraic equations on sets of patterns anchored on a protein of the
  * same type.
  *
  * This is the case when one wonders in what context a mechanistic
  * interaction can or cannot occur, when one wonders about which contexts a
  * rule instance can add or remove an instance of another rule.
  *
  * We will propose here an encoding of the sets of contexts (or of an
  * equivalent of a set of patterns) built like refinement of the same
  * instance of protein, the anchor, in a formula in a propositional logic.
  *
  * The usual logical combiners will make it possible to combine formulas to
  * form more complex ones.
  *
  * Lower closure operators will be proposed to specialize a formula with
  * only the description of the patterns of site-graphs (the logic being
  * more permissive) and to take into account the result of a static
  * analysis.
  *
  * Finally, an implementation based on binary decision diagrams will allow
  * the efficient resolution and the translation of these formulas into
  * canonical sets of contexts.
  *)

(** Anchor and agents naming
  * ------------------------
  *
  * A $\kappa$ pattern is a connected site-graph of agents.
  *
  * A site-graph is an undirected graph were each ends of edges are
  * *“sites”*. Sites are labeled.
  *
  * An *“agent”* is just a node of the site-graph. The edge between two
  * nodes could be interpreted here as an interaction between two
  * biochemical agents.
  *
  * So, to start modeling untyped site-graphs without states, once we
  * consider only sites names, we can use an equivalence between an
  * undirected edge with two sites and two complementary directed edges
  * labeled with their *“input”* sites.
  *
  * ***N.B.** This is a reduced model of what $\kappa$ site-graphs are, but
  * we will wait a bit before introducing sites states and typed agents:
  * these concepts are sufficiently independent to let us build without them
  * a lot of axioms that will still be true in our full logic!*
  *
  * We define a unique agent as the *“anchor”* (it could be also called the
  * *“root”* of our pattern). We will color the anchor node in [red]{} in
  * all our figures.
  *
  * What we get here is a directed multi-graph (there could have several
  * edges between two nodes). This will be a really much clearer notation
  * since we will deal a lot with concepts like paths and directed cycles!
  *
  * We derive from a pattern with an anchor the set of all paths from this
  * anchor $P$. Since our multigraph contains cycles, $P$ isn’t finite.
  * This is not an issue, we only need a finite number of paths to describe
  * without ambiguity our graph and any redondant path will be reduced in
  * the close form of our formula (cf. Semantics).
  *
  * The list made of the concatenation of input site names of all edges
  * traversed by a path is used to name the agent at the end of the path.
  *
  * To be a valid representation of site-graphs without types and states our
  * directed multi-graphs has to follow two structural constraints:
  *
  * -   if there is a directed edge from $x$ to $y$, there have to be a
  *     directed edge from $y$ to $x$,
  *
  * -   if $x$ and $x'$ are two edges starting from the same node, they
  *     should have different labels:
  *
  * Since two edges starting from the same node couln’t have the same
  * identifier this sequence determine without ambiguity an unique node of
  * our graph.
  *
  * The anchor is named $\epsilon$.
  *)


(** 'c_' prefix stands for ckappa objects *)
type c_type = int

(** from KaSa_rep/frontend/ckappa_sig.ml *)
type c_state = int
type c_site_name = int

type c_binding = Bounded of c_site_name | Free | Unknown
type c_site = c_binding (* TODO: c_binding * c_state *)
type c_path = (c_site * c_site) list

(** TODO: Polymorphic structure for handling lemma, corresponding set of lemma to integer:
  *
  * type 'site_graph lemma =
  *   {
  *     hyp : 'site_graph ;
  *     refinement : 'site_graph list
  *   }
  *)

type c_hyp = int
type c_refinement = int
(* type c_site_graph = int (* TODO: Site_graphs.KaSa_site_graph *) *)


(** MVBDU data-structure to handle logic formulas
  * =============================================
  *
  * Here we will use MVBDU as binary decision diagrams.
  * MVBDU are sets of finite maps from integers to integers.
  *)

module Formula_bdu :
  Mvbdu_wrapper.Mvbdu
  with type key = c_hyp
   and type value = c_refinement
  with type mvbdu = Mvbdu_wrapper.Mvbdu.mvbdu =
  Mvbdu_wrapper.Mvbdu

module Formula_intbdu = Mvbdu_wrapper.Internalize (Formula_bdu)

(** This extend module of internalized MVBDU with two maps two encode lemma:
  * - int -> hypotesis ('site_graph)
  * - int -> refinement lemma ('site_graph list) *)

module Formula_G = Mvbdu_wrapper.Generalize (Formula_intbdu)

(* Rely on a standard libary map https://ocaml.org/learn/tutorials/map.html *)
(* let lemma_to_int (_lemma : 'site_graph Public_data.lemma) =
    let _hyp = Public_data.get_hyp _lemma in
    let _refinement = Public_data.get_refinement _lemma in
    0 *)


(** 'm_' prefix stands for sub-formulas in MVBDU *)
let m_bool x =
  if x then Formula_G.mvbdu_true () else Formula_G.mvbdu_false ()

let m_equal a b = m_bool (a = b)

let m_imply = Formula_G.mvbdu_imply

let m_equiv = Formula_G.mvbdu_equiv

let m_and = Formula_G.mvbdu_and

let m_or = Formula_G.mvbdu_and

let m_forall set formula =
  List.fold_left m_and (m_bool true) (List.map formula set)

let m_exist set formula =
  List.fold_left m_or (m_bool false) (List.map formula set)

(** Operators are encapsulated in a submodule to prevent the overwriting of
  * other defined operators when this module is open *)

module Logical_operators = struct
  let ( === ) = m_equal
  let ( <=> ) = m_equiv
  let (  => ) = m_imply
end

open Logical_operators

(* TODO: Good domain support for handling logic formula, this one is static (should be replace by dynamic one) *)

(** 'g_' prefix stands for site-graph *)
type g_static_support =
  { g_paths: c_path list
  ; g_aliases: c_path -> c_path list 
  ; g_sites: c_type -> c_site list
  ; g_state: c_path -> c_site -> c_state
  ; g_states: c_type -> c_site -> c_state list
  ; g_type: c_path -> c_type
  ; g_types: c_type list }

(* TODO: To ensure that we have the same context `c` on most operation, we can init everything with a (mutable?) context *)


(** Predicate encoding (TODO: move this in Formula_G functor) *)

type encoded_predicates = Agent | Alias | Site | Type
type encoded_values = Path of c_path | Site of c_site | Type of c_type

module PredicateToIntMap = Map.Make (struct
  type t = encoded_predicates * (encoded_values list) 
  let compare = compare
end)

let global_m = ref PredicateToIntMap.empty
let global_key = ref 0

let rec getKey (value : encoded_predicates * (encoded_values list)) : int =
  try PredicateToIntMap.find value !global_m
  with _ ->
    global_m := PredicateToIntMap.add value !global_key !global_m ;
    global_key := !global_key + 1 ;
    getKey value

(* TODO: Error: Unbound value Formula_G.getKey *)
(* let _ = Formula_G.getKey *)

let predicate_to_mvbdu (predicate: encoded_predicates) (values: encoded_values list) =
  let value = 0 in (* TODO: It should handle different contexts *)
  let key = getKey (predicate, values) in

  Formula_G.mvbdu_of_association_list [ (key, value) ]

(* TODO: is the reverse map useful? *)
(*
module IntToPredicateMap = Map.Make (struct
  type t = int
  let compare = compare
end)

let m' = ref IntToPredicateMap.empty
*)


(** First order propositional classical logic (LK) for untyped anchored site-graphs
  * ===============================================================================
  *
  * We explicitly declare all our agents by the logical predicate:
  *
  * Predicate 1.A: $agent(p) \text{ with } p \in P$
  *)

(* Predicates could either be encoded in the MVBDU or evaluate to a boolean value *)

(** 'p_' prefix stands for predicate *)
let p_agent = fun (p : c_path) ->
  predicate_to_mvbdu Agent [Path(p)]

let p_agent_eval c = fun (p : c_path) ->
  m_exist c.g_paths (fun x -> x === p)

(** ### Parent constraint
  *
  * We ensure that every agent in a path from an anchor to our agent is
  * defined in our pattern by the axiom:
  *
  * Axiom 1: $\forall \lambda \in P, \lambda.u \in P, agent(\lambda.u) \implies agent(\lambda)$
  *
  * . is used as concatenation operator, defined on paths, e.g: $xy.z \iff xyz$.
  *
  * The anchor identifier: $\epsilon$ is the “empty list”,
  * $\epsilon.\lambda \iff \lambda \iff \lambda.\epsilon$
  *)

let axiom_1 c =
  m_forall c.g_paths (function
    | [] -> m_bool true
    | u :: lambda -> p_agent (u :: lambda) => p_agent lambda )


(** From tree to graph representation
  * ---------------------------------
  *
  * We want to encode the missing information to obtain a graph from a list of paths
  * inside our logic with a new predicate:
  *
  * Predicate 2: $alias(u, v) \text{ with } (u, v) \in P^2$
  *)

let p_alias = fun (u : c_path) (v : c_path) ->
  predicate_to_mvbdu Alias [Path(u); Path(v)]

let p_alias_eval c = fun (u : c_path) (v : c_path) ->
  m_exist (c.g_aliases u) (fun x -> x === v)

(** An alias definition requires both agents engaged to be set:
  *
  * Axiom 2: \forall (u, v) \in P^2, alias(u, v) \implies agent(u) \land agent(v)$
  *)

let axiom_2 c =
  m_forall c.g_paths (fun u ->
      m_forall c.g_paths (fun v -> p_alias u v => m_and (p_agent u) (p_agent v))
  )

(** Arguments in aliases predicates, indeed, commutes:
  *
  * Axiom 3: $\forall (u, v) \in P^2, $alias(u, v) \iff alias(v, u)$
  *)

let axiom_3 c =
  m_forall c.g_paths (fun u ->
      m_forall c.g_paths (fun v -> p_alias u v <=> p_alias v u) )

(** And $alias$ property is transitive:
  *
  * Axiom 4: $\forall (u, v, w) \in P^3, alias(u, v) \land alias(v, w) \implies alias(u, w)$
  *)

let axiom_4 c =
  m_forall c.g_paths (fun u ->
      m_forall c.g_paths (fun v ->
          m_forall c.g_paths (fun w ->
              m_and (p_alias u v) (p_alias v w) => p_alias u w ) ) )

(** Every agent containing in its path one or several agents which have
  * aliases, should itself have at least the same number of aliases:
  *
  * Axiom 5: $\forall (\lambda, \lambda', \lambda.u, \lambda'.u) \in P^4, alias(\lambda, \lambda') \implies alias(\lambda.u, \lambda'.u)$
  *)

let axiom_5 c =
  m_forall c.g_paths (function
    | [] -> m_bool true
    | u :: lambda ->
        m_forall c.g_paths (function
          | [] -> m_bool true
          | u' :: lambda' ->
              m_and (u === u') (p_alias lambda lambda')
              => p_alias (u :: lambda) (u :: lambda') ) )

(** Axiom 6: $\forall (\lambda, \lambda', \lambda.u.v, \lambda'.v.u) \in P^4, agent((\lambda.u.v, \lambda'.v.u)) \implies alias((\lambda.u.v, \lambda'.v.u), (\lambda, \lambda'))$
  *)

let axiom_6 c =
  m_forall c.g_paths (function
    | [] | [(_, _)] -> m_bool true
    | (u, v) :: (v', u') :: lambda ->
        m_and (u === u') (v === v')
        => p_alias lambda ((u, v) :: (v', u') :: lambda) )

(** Every path is its own alias:
  *
  * Axiom 7: $\forall p \in P, alias(p, p) \implies \top$
  *)

let axiom_7 c = m_forall c.g_paths (fun p -> p_alias p p => m_bool true)


(** Agent sites
  * -----------
  *
  * ### Site bindings
  *
  * Agent sites could be either free, unspecified or bounded.
  *
  * Bounded sites are implicitly defined by agent names when the agent bounded is included in the pattern (cf. Axiom 12).
  *
  * When it’s not the case, we need a predicate:
  *
  * Predicate 3.A: $site(p, s, t) \text{ with } p \in P, s \in Sites(p), t \in Sites(p.s): \text{Sites}$
  *)

let p_site = fun (p : c_path) (s : c_site) (t : c_site) ->
  predicate_to_mvbdu Site [Path(p); Site(s); Site(t)]

let p_site_eval c = fun (p : c_path) (s : c_site) (t : c_site) ->
  m_and (m_exist (c.g_sites @@ c.g_type p) (fun x -> x === s))
        (m_exist (c.g_sites @@ c.g_type p) (fun x -> x === t))

(** Here $s$ stands for “source” and $t$ stands for “target”. *)

(** Each site of an agent couldn’t be bounded simultaneously to several
  * agents:
  *
  * Axiom 9: $\forall p \in P, \forall s \in Sites(p), \forall (t, t') \in Sites(p.s)^2, site(p, s, t) \land site(p, s, t') \implies (t = t')$
  *
  * Here $=$ means equality in the sens of elements in sets theory, which
  * lets us read $a=b$ with $(a, b) \in S^2$ as $a$ and $b$ refer to the
  * same element in the set $Sites$.
  *)

let axiom_9 c =
  m_forall c.g_paths (fun p ->
      m_forall
        (c.g_sites @@ c.g_type p)
        (fun s ->
          m_forall
            (c.g_sites @@ c.g_type ((s, Unknown) :: p))
            (fun t ->
              m_forall
                (c.g_sites @@ c.g_type ((s, Unknown) :: p))
                (fun t' -> m_and (p_site p s t) (p_site p s t') => (t === t'))
              ) ) )

(** Site states are shared by aliases:
  *
  * Axiom 10:
  * $\forall (u, v) \in P^2, \forall s \in Sites(u), \forall t \in Sites(u.s), site(u, s, t) \land alias(u, v) \implies site(v, s, t)$
  *)

let axiom_10 c =
  m_forall c.g_paths (fun u ->
      m_forall c.g_paths (fun v ->
          m_forall
            (c.g_sites @@ c.g_type u)
            (fun s ->
              m_forall
                (c.g_sites @@ c.g_type ((s, Unknown) :: u))
                (fun t -> m_and (p_site u s t) (p_alias u v) => p_site v s t)
              ) ) )

(** A $site$ predicate requires an $agent$ one:
  *
  * Axiom 11:
  * $\forall p \in P, \forall s \in Sites(p), \forall t \in Sites(p.s), site(p, s, t) \implies agent(p)$
  *)

let axiom_11 c =
  m_forall c.g_paths (fun p ->
      m_forall
        (c.g_sites @@ c.g_type p)
        (fun s ->
          m_forall
            (c.g_sites @@ c.g_type ((s, Unknown) :: p))
            (fun t -> p_site p s t => p_agent p) ) )

(** The definition of an agent (except for the anchor), requires by
  * construction:
  *
  * Axiom 12.A:
  * $\forall \lambda \in P, \forall u \in Sites(\lambda), agent(\lambda.u) \implies \exists s \in Sites(\lambda.u), site(\lambda, u, s)$
  *)

(** $\mathfrak{F} \in S$: Sites set contains a “free” site.
  *
  * This doesn’t have to be defined by a special predicate: a free site in
  * $\kappa$ is encoded with a binding to the $\mathfrak{F}$ site.
  *)

(** But we have to ensure that this site isn’t taken by any path:
  *
  * Axiom 13:
  * $\forall (u, u.s.\lambda) \in P^2, \forall s \in Sites(u), site(u, s, \mathfrak{F}) \land agent(u.s.\lambda) \implies \bot$
  *)

let axiom_13 c =
  m_forall c.g_paths (function
    | [] | [_] -> m_bool true
    | lambda :: s :: u ->
        let x, _ = s in
        m_and (p_site u x Free) (p_agent (s :: lambda :: u)) => m_bool false )


(** ### Site states
  *
  * We want to encode the possibility of a site to also have a state, so we
  * update our previous state predicate (3.A):
  *
  * Predicate 3.B: $site(p, s, s_i): p \in P, s \in Sites: \text{Sites,} s_i \in Sites \times States: \text{``Binding'' Sites} \times \text{States}$
  *)

(** Like a site can’t bind several agents, a site can’t have several states
  * (but a site can both have a state and bind an agent):
  *
  * Axiom 9.B: $\forall p \in P, \forall s \in Sites(p), \forall (s_i, s_j) \in (Sites(p.s) \times States(p))^2, site(p, s, s_i) \land site(p, s, s_j) \implies (s_i = s_j)$
  *)

(** /!\ Here we update the c_site type constructor to handle states softly *)

(** The previous notation uses values of the abstracted $Site \times State$
  * “tuple” (Cartesian product). It is equivalent to write it in the
  * following “expensed” form:
  *
  * $\forall p \in {P}, \forall s \in Sites, \forall ((t, r), (t', r')) \in (Sites(p.s) \times States(p))^2, site(p, s, (t, r)} \land site(p, s, (t', r')) \implies (t = t') \land (r = r')$
  *)


(** Types! (of agents and site bindings)
  * ====================================
  *
  * Typed agents
  * ------------
  *
  * Agents are biochemical objects ; two objects of the same biochemical
  * “species” share the same type.
  *
  * Predicate 1.B: $type(p, t) \text{ with } p \in P \text{ and } t \in T: \text{Types}$
  *)

let p_type = fun (p : c_path) (t : c_type) ->
  predicate_to_mvbdu Type [Path(p); Type(t)]

let p_type_eval c = fun (p : c_path) (t : c_type) ->
  c.g_type p === t

(** We introduce a $type$ predicate in order to replace the $agent$ one:
  *
  * Axiom 15: $\forall p \in P, agent(p) \implies \exists t \in T, type(p, t)$
  *)

let axiom_15 c =
  m_forall c.g_paths (fun p ->
      p_agent p => m_exist c.g_types (fun t -> p_type p t) )

(** We can see the $type$ predicate as agent declaration:
  *
  * Axiom 16: $\forall p \in P, \forall t \in T, type(p, t) \implies agent(p)$
  *)

let axiom_16 c =
  m_forall c.g_paths (fun p ->
      m_forall c.g_types (fun t -> p_type p t => p_agent p) )

(** Each agent is habited by a unique type, we enforce this rule by the
  * axiom:
  *
  * Axiom 17: $\forall u \in P, \forall (a, b) \in T^2, type(u, a) \land type(u, b) \implies (a = b)$
  *)

let axiom_17 c =
  m_forall c.g_paths (fun u ->
      m_forall c.g_types (fun a ->
          m_forall c.g_types (fun b ->
              m_and (p_type u a) (p_type u b) => (a === b) ) ) )

(** Types are shared by aliases:
  *
  * Axiom 18:
  * $\forall u, u' \in P^2, \forall t \in T, alias(u, u') \land type(u, t) \implies type(u', t)$
  *)

let axiom_18 c =
  m_forall c.g_paths (fun u ->
      m_forall c.g_paths (fun u' ->
          m_forall c.g_types (fun t -> p_type u t => p_type u' t) ) )


(** Typed site bindings
  * -------------------
  *
  * Site binding are typed: they encode the type of agents which could bind
  * to the site.
  *
  * The states $s_i \in {States}(s)$ which could take a site is a
  * function of the site itself $s \in Sites(t)$.
  *
  * This is actually the purpose of typing, to ensure that agents of the
  * same type have the same sites.
  *
  * Predicate 3.C: $site(p, s, x \times (s' \times t' + \mathfrak{F})) \text{ with } p \in {P}, \forall t \text{ as } type(p, t), s \in Sites(t), x \in States(s)$
  *
  * $t'$, respectively $s'$, is the agent type, respectively the site name,
  * of the agent bounded, but as the agent is not forced to be in our
  * pattern it could be not present in $T$, respectively the set of all site
  * names of all agents of our pattern.
  *
  * $+$ is the “type sum” operator. It’s here equivalent to a set union,
  * it has less precedence than the cartesian product.
  *
  * So, as $\mathfrak{F}$ is defined here inside our predicate, to be
  * consistent we have now the constraints
  * $\forall t \in {T}, \forall s \in Sites(t), s \neq \mathfrak{F}$ and
  * $s' \neq \mathfrak{F}$.
  *)

(** We can rewrite the Axiom 12.A with more accuracy by making an inference
  * on binding type:
  *
  * Axiom 12.B: $\forall \lambda \in P, \forall u \in Sites(\lambda), \forall t \text{ as } type(\lambda, t), \forall u \in Sites(t), type(\lambda.u, t) \implies \exists s_i \in Sites(t), \exists x \in {States}(s_i), site(\lambda, u, (x, s_i, t))$
  *)

let axiom_12 c =
  m_forall c.g_paths (fun lambda ->
      m_forall
        (c.g_sites @@ c.g_type lambda)
        (fun _ ->
          let t = c.g_type lambda in
          m_forall (c.g_sites t) (fun u ->
              p_type ((u, Unknown) :: lambda) t
              => m_exist (c.g_sites t) (fun s_i ->
                     m_exist (c.g_states t s_i) (fun _ -> p_site lambda u (* x t *) s_i) (* TODO *)
                 ) ) ) )

(** So, we also have the reverse constraint:
  *
  * Axiom 19: $\forall \lambda \in P, \forall u \in Sites(\lambda), \forall t \in T, \forall u \in Sites(t), \forall x \in States(u), site(\lambda.u, (x, u, t)) \implies type(\lambda, t)$
  *)

let axiom_19 c =
  m_forall c.g_paths (fun lambda ->
      let t = c.g_type lambda in
      m_forall (c.g_sites t) (fun u ->
          m_forall (c.g_states t u) (fun _ ->
              p_site ((u, Unknown) :: lambda) u (* x t *) u => p_type lambda t ) ) ) (* TODO *)


(** STRUCTURAL AXIOMS **)

let axioms c =
  List.fold_left m_and (m_bool true)
    [ axiom_1 c (* agent parent constraint *)
    ; axiom_2 c (* alias requires agents *)
    ; axiom_3 c (* alias commutativity *)
    ; axiom_4 c (* alias transitivity *)
    ; axiom_5 c (* alias propagation *)
    ; axiom_6 c (* alias loop *)
    ; axiom_7 c (* alias identity *)
      (* TODO: axiom_8 *)
    ; axiom_9 c (* agent site couldn’t be bounded to several agents *)
    ; axiom_10 c (* aliases have same site states *)
    ; axiom_11 c (* site requires agent *)
    ; axiom_12 c (* inference on binding type *)
    ; axiom_13 c (* `Free` site shouldn't be take by any path *)
      (* TODO: axiom_14 *)
    ; axiom_15 c (* an agent have a type *)
    ; axiom_16 c (* type requires agent *)
    ; axiom_17 c (* an agent is habited by a unique type *)
    ; axiom_18 c (* aliases have same types *)
    ; axiom_19 c
    ]

(** Semantics
  * =========
  *
  * Previous axioms define general truth properties our logic should holds.
  *
  * If some of them are not respected our logic should be evaluated to the
  * $\bot$ value.
  *
  * To ensure this, we have to define reduction rules that will be applied
  * successively to our formula to converge to a closed result. This
  * operation is named a closure, it’s a fixed point we obtained when we
  * can’t apply any rule on a formula.
  *)

(** We work with formulas in normal conjunctive form, which are considered
  * equivalent no matter the order of terms $A \land B \iff B \land A$.
  *
  * Here variables $x$, $a$, $b$ stands for any logical (sub)formula:
  *)

(** Top and bottom:
  *
  * $x \land \top \hookrightarrow x$
  *
  * $x \land \bot \hookrightarrow \bot$
  *
  * $x \or \top \hookrightarrow \top$
  *
  * $x \or \bot \hookrightarrow \bot$
  *)

(** Identities and negations:
  *
  * $x \land \neg x \hookrightarrow \bot$
  *
  * $x \land x \hookrightarrow x$
  *)

(** Implications:
  *
  * $a \iff b \hookrightarrow (a \implies b) \land (b \implies a)$
  *
  * $(a \implies b) \land a \hookrightarrow a \land b$
  *
  * $(a \implies b) \land \neg a \hookrightarrow \top$
  *)

(** Rewriting for path $P' \subset P$ without alias loop -> FixPoint
  *
  * $alias(\lambda, \lambda.p) \hookrightarrow \top$
  *
  * or keep unitary/simple cycles?
  *
  * $agent(\lambda.p.p) \land alias(\lambda.p.p, \lambda) \hookrightarrow alias(\lambda.p, \lambda)$
  *
  * how to handle alias redondancies/duplicates rules?
  *
  * $alias(\lambda, \lambda) \hookrightarrow \top$
  *
  * $agent(\lambda.x.y) \land alias(\lambda.x.y, \lambda) \land alias(\lambda.x, \lambda) \hookrightarrow alias(\lambda.x, \lambda) \land alias(\lambda.y, \lambda)$
  *)


(** UTILS DATA-STRUCTURE CONVERSION FUNCTIONS **)

let ckappa_to_mvbdu (c : Ckappa_sig.kappa_handler) =
  let _ = c.agents_dic in
  (* : agent_dic; *)
  (* : (unit,unit) Dictionary_of_agents.dictionary *)
  let _ = c.interface_constraints in
  (* : agent_specification Int_storage.Nearly_inf_Imperatif.t; *)
  let _ = c.sites in
  (* : site_dic Int_storage.Nearly_inf_Imperatif.t; *)
  let _ = c.states_dic in
  (* : state_dic Int_storage.Nearly_inf_Imperatif.t Int_storage.Nearly_inf_Imperatif.t *)
  
  (* From KaSa_rep/frontend/ckappa_sig.ml *)
  (* DEBUG.. Little temporarily hack for print --debug informations: KaSa_rep/frontend/print_ckappa.ml *)

  m_bool true (* axioms c *)


let cckappa_to_mvbdu parameters error (handler : Cckappa_sig.kappa_handler) =

  (* DEBUG.. Cckappa handler structure: *)
  let _ : (string * Ckappa_sig.position list) Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t = handler.agents_annotation in (* int *)

  let _ : Ckappa_sig.agent_specification Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t  = handler.interface_constraints in (* int *)
  let _ : Ckappa_sig.site_dic Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.t = handler.sites in (* int *)
  let _ : Ckappa_sig.state_dic Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif.t = handler.states_dic in (* int ^ 2 *)

  (* ** Encode agents, states & sites in MVBDU predicates *)
  let encode_agent agent_name _ (* string_of_agent_name *) =
    let _ (* id *) = agent_name in
    let _ = Graphs.paths_from_anchor in (* TODO *)
    ()
  in

  let encode_state _ (* parameters *) state =
    let _ (* id *) = match state with
    | Ckappa_sig.Internal a ->
        (* Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "[not implemented yet] %s%s" (* TODO *)
          (Remanent_parameters.get_prefix parameters) *)
          a (* TODO *)
    | Ckappa_sig.Counter a ->
        (* Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "[not implemented yet] %s%i" (* TODO *)
          (Remanent_parameters.get_prefix parameters) *)
          a (* TODO *)
    | Ckappa_sig.Binding Ckappa_sig.C_Free ->
        (* Loggers.fprint^
          (Remanent_parameters.g^t_logger parameters)
          "[not implemented yet] %sfree" (* TODO *)
          (Remanent_parameters.get_prefix parameters) *)
          "free" (* TODO *)
    | Ckappa_sig.Binding (Ckappa_sig.C_Lnk_type (a, b)) ->
        (* Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "[not implemented yet] %sagent_type:%s@@site_type:%s" (* TODO *)
          (Remanent_parameters.get_prefix parameters) *)
          (Ckappa_sig.string_of_agent_name a) ^ " -> " ^
          (Ckappa_sig.string_of_site_name b)
    in (* TODO *)
    ()
  in

  let encode_site _ (* parameters *) site =
    let _ (* id *) = match site with
    | Ckappa_sig.Internal a ->
        (* Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "[not implemented yet] %s%s(internal state)"
          (Remanent_parameters.get_prefix parameters) *)
        a (* TODO: p_site = c_path -> c_site -> c_site -> Formula_G.mvbdu *)
    | Ckappa_sig.Counter a ->
        (* Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "[not implemented yet] %s%s(counter value)"
          (Remanent_parameters.get_prefix parameters) *)
        a (* TODO: p_site = c_path -> c_site -> c_site -> Formula_G.mvbdu *)
    | Ckappa_sig.Binding a ->
        (* Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "[not implemented yet] %s%s(binding state)"
          (Remanent_parameters.get_prefix parameters) *)
        a (* TODO: p_site = c_path -> c_site -> c_site -> Formula_G.mvbdu *)
    in (* TODO *)
    ()
  in

  (* ** Debug print utilities *)
  let debug_print_f _ (* print_aux *) parameters error i site () () =
    let _ (* parameters *) =
      Remanent_parameters.update_prefix parameters
        ("[DEBUG] site_type:" ^ Ckappa_sig.string_of_site_name i ^ "->")
    in
    let _ = site in (* <--------------------------------------------- *)
    (* print_aux parameters site ; *)
    (* Loggers.print_newline log ; *)
    error
  in
  let debug_print_state_f _ (* print_aux *) parameters error i state () () =
    let _ (* parameters *) =
      Remanent_parameters.update_prefix parameters
        ("[DEBUG] state_id:" ^ Ckappa_sig.string_of_state_index i ^ "->")
    in
    let _ = state in (* <--------------------------------------------- *)
    (* print_aux parameters state ; *)
    (* Loggers.fprintf (Remanent_parameters.get_logger parameters) "\n" ; *)
    error
  in

  (* ** Logging *)
  (* let log = Remanent_parameters.get_logger parameters in *)
  (* Loggers.fprintf log "%s" (Remanent_parameters.get_prefix parameters) ; *)
  (* Loggers.print_newline log ; *)
  (* let parameters_agent =
    Remanent_parameters.update_prefix parameters "agents:"
  in *)
  (* Loggers.fprintf log "%s" (Remanent_parameters.get_prefix parameters_agent) ; *)
  (* Loggers.print_newline log ; *)

  (* ** Iterate through Cckappa structure *)
  (* ** + Agents *)
  let _ (* TODO *) = Ckappa_sig.Dictionary_of_agents.iter parameters error
    (fun _parameters error i agent_name () () ->
      encode_agent agent_name (Ckappa_sig.string_of_agent_name i) ;
      error
    )
    handler.agents_dic
  in
  (* let error = (* ALREADY DONE !! *)
    Ckappa_sig.Dictionary_of_agents.iter parameters error
      (fun _parameters error i agent_name () () ->
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "[not implemented yet] %sagent_type:%s:%s"
          (Remanent_parameters.get_prefix parameters)
          (Ckappa_sig.string_of_agent_name i)
          agent_name ;
        Loggers.print_newline
          (Remanent_parameters.get_logger parameters) ;
        error )
      handler.Cckappa_sig.agents_dic
  in *)

  (* ** + Sites *)
  (* let parameters_sites =
    Remanent_parameters.update_prefix parameters "sites:"
  in *)
  (* Loggers.fprintf log "%s" (Remanent_parameters.get_prefix parameters_sites) ; *)
  (* Loggers.print_newline log ; *)
  let _ (* error *) =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.print
      parameters error
      (fun parameters error a ->
        let error =
          Ckappa_sig.Dictionary_of_sites.iter parameters error
            (debug_print_f encode_site) a
        in
        error )
      handler.Cckappa_sig.sites
  in

  (* ** + States *)
  (* let parameters_states =
    Remanent_parameters.update_prefix parameters "states:"
  in *)
  (* Loggers.fprintf log "%s \n"
    (Remanent_parameters.get_prefix parameters_states) ; *)
  let _ (* error *) =
    Ckappa_sig.Agent_type_site_nearly_Inf_Int_Int_storage_Imperatif_Imperatif
    .print parameters error
      (fun parameters error a ->
        Ckappa_sig.Dictionary_of_States.iter parameters error
          (debug_print_state_f encode_state)
          a )
      handler.Cckappa_sig.states_dic
  in

  (* ** + TODO: Duals *)
  (* let parameters_duals =
    Remanent_parameters.update_prefix parameters "duals:"
  in *)
  (* Loggers.fprintf log "%s \n"
    (Remanent_parameters.get_prefix parameters_duals) ; *)
  (* let _ (* error *) =
    Ckappa_sig
    .Agent_type_site_state_nearly_Inf_Int_Int_Int_storage_Imperatif_Imperatif_Imperatif
    .print parameters error
      (fun parameters error (a, b, (c : Ckappa_sig.c_state)) ->
        let _ =
          Loggers.fprintf log "[not implemented yet] %sagent_type:%s,site_type:%s,state_id:%s\n"
            (Remanent_parameters.get_prefix parameters)
            (Ckappa_sig.string_of_agent_name a)
            (Ckappa_sig.string_of_site_name b)
            (Ckappa_sig.string_of_state_index c)
        in
        error )
      handler.Cckappa_sig.dual
  in *)

  (* TODO: let _ = Graphs.paths_from_anchor graph anchor in *)
  (* From KaSa_rep/more_datastructures/graphs.ml *)

  m_bool true


let lkappa_to_mvbdu _ (* : TODO *) = m_bool true


(** TODO: UTILS TO INCREMENTALY UPDATE A MVBDU ? **)


(** Main logic of the module, this function is called in KaSa_rep/main/KaSa.ml
  *
  * TODO: This should be integrated after the accessibility analysis:
  * -> KaSa_rep/influence_map/algebraic_construction.ml 
  * -> KaSa_rep/remanent_state/remanent_state.ml *)

let main (* parameters *) (* error *) handler (* state *) =

  let error = Exception.empty_error_handler in
  let parameters = Remanent_parameters.get_parameters
        ~called_from:Remanent_parameters_sig.KaSa () in
  Exception.print parameters error ;
  let log = (Remanent_parameters.get_logger parameters) in
  Loggers.fprintf log "------------------------------------------------------------" ;
  Loggers.print_newline log ;
  Loggers.fprintf log "* Logic encoding" ;
  Loggers.print_newline log ;


  (*** STEP 0: Initialize MVBDU ***)

  (* WRAPPED MVBDU *)
  (* let _ (* error *), _ (* handler_bdu *) =
    if Formula_bdu.is_init () then Formula_bdu.get_handler parameters error
    else Formula_bdu.init parameters error
  in *)

  (* WRAPPED MVBDU INTERNALISED *)
  let () = Formula_G.init parameters in (* TODO: Still get error: file_name: KaSa_rep/abstract_domains/mvbdu/mvbdu_wrapper.ml; message: line 778; exception:Exit *)
  let initialized = Formula_G.is_init () in (* Fixed error: file_name: KaSa_rep/abstract_domains/mvbdu/mvbdu_wrapper.ml; message: line 804; exception:Exit *)
  Loggers.fprintf log "[DEBUG] MVBDU is initialized? -> %b" initialized ; (* TODO *)
  Loggers.print_newline log ;


  (*** STEP 1: Encode (CKappa, CcKappa, LKappa) site-graphs inside MVBDU *~*)

  (* Get handler from Export_to_KaSa.get_data state which is an option type get of a Cckappa_sig.kappa_handle~
     and encode it into a Formula_G.mvbdu *)
  let m = match handler with
    | None -> m_bool false
    | Some c -> (
      Loggers.fprintf log "~~~~~~~~~~~~~~~~ Cckappa_sig.kappa_handler ~~~~~~~~~~~~~~~~~" ;
      let _ = Print_handler.print_handler parameters error c in
      (* Print output or minimal working example of interest (see below in STEP 2):
       *
       * ```
       * agents:
       * agents:agent_type:0:A
       * sites:
       * sites:0:
       * sites:0:site_type:0->x(internal state`
       * sites:0:site_type:1->x(binding state)
       * states:
       * states:0:
       * states:0:0:
       * states:0:0:state_id:0->u
       * states:0:0:state_id:1->p
       * states:0:1:
       * states:0:1:state_id:0->free
       * states:0:1:state_id:1->agent_type:0@site_type:1
       * duals:
       * duals:0:
       * duals:0:1:
       * duals:0:1:1:
       * duals:0:1:1:agent_type:0,site_type:1,state_id:1
       * ```
       *)

      (* Construct MVBDU *)
      Loggers.fprintf log "~~~~~~~~~~~~~~~~~ [DEBUG] cckappa_to_mvbdu ~~~~~~~~~~~~~~~~~" ;
      Loggers.print_newline log ;
      cckappa_to_mvbdu parameters error c
    )
  in

  Loggers.fprintf log "~~~~~~~~~~~~~~~~~~~~~~ Formula_G.mvbdu ~~~~~~~~~~~~~~~~~~~~~" ;
  Loggers.print_newline log ;

  (* Construct list of variables *)
  let v = Formula_G.variables_list_of_mvbdu m in

  (* Print *)
  Formula_G.print parameters m ;
  Formula_G.print_variables_list parameters v ;


  (*** (BONUS) STEP 2: Cloture by the structural lemma defined in the logic ***)

  (* UNIT TESTS:
   *************
   *
   * Minimal working example of interest:
   *
   * A[.] - A[.] -> A | A
   * A[p] - A[p] -> A | A
   * A[.] - A[p] -> A | A (which is the same than A[p] - A[.] -> A | A)
   * --------------------
   * A[_] - A[_] -> A | A
   *
   * The purpose here is to find the rights rules for reducing formula from a disjonction of this 3 rules.
   * With the fact we have all existing possible cases we can apply an inverse of refinement (a kind of unification)!
   *
   * The same example written in Kappa 4 syntax:
   * 
   * ```
   * /* Signatures */
   * %agent: A(x{u p}) // Declaration of agent A with 1 modifiable site x
   *
   * /* Rules */
   * A(x{p}[./1]), A(x{p}[./1]) @k // A{p} binds A{p}
   * A(x{u}[./1]), A(x{p}[./1]) @k // A{u} binds A{p}
   * A(x{u}[./1]), A(x{u}[./1]) @k // A{u} binds A{u}
   * 
   * /* Variables */
   * %var: k 1.0E-3 // per molecule per second
   * %var: n 1000
   *
   * /* Initial conditions */
   * %init: n A(x{u})
   * %init: n A(x{p})
   * ```
   *
   * Here is a hard-coded intermediate representation of this example: *)

  let test_1 : g_static_support =
    { g_paths= [[]; [(Bounded 0 (*x*), Bounded 0 (*x*))]]
    ; g_sites= ( function 0 (*A*) -> [Bounded 0 (*x*)] | _ -> [] )
    ; g_state=
        (function
        | [] -> ( function Bounded 0 (*x*) -> 0 (*u*) | _ -> 0 )
        | [(Bounded 0 (*x*), Bounded 0 (*x*))] -> (
            function Bounded 0 (*x*) -> 0 (*u*) | _ -> 0 )
        | _ -> fun _ -> 0)
    ; g_states=
        (function
        | 0 (*A*) -> (
            function Bounded 0 (*x*) -> [0 (*u*); 1 (*p*)] | _ -> [] )
        | _ -> fun _ -> [] )
    ; g_type=
        (function
        | [] -> 0 (*A*)
        | [(Bounded 0 (*x*), Bounded 0 (*x*))] -> 0 (*A*)
        | _ -> 0)
    ; g_types= [0 (*A*)]
    ; g_aliases= fun _ -> [] }
  in
  let m = (* DEBUG: *) axiom_1 (* axioms *) test_1 in

  (* Versus the same Kappa program but without writing explicitly alls possible refinements:
   *
   * ```
   * /* Signatures */
   * %agent: A(x{u p}) // Declaration of agent A with 1 modifiable site x
   *
   * /* Rules */
   * A(x[./1]), A(x[./1]) @k // A binds A
   *
   * /* Variables */
   * %var: k 1.0E-3 // per molecule per second
   * %var: n 1000
   `
   * /* Initial conditions */
   * %init: n A(x{u})
   * %init: n A(x{p})
   * ```
   *)

  Loggers.fprintf log "~~~~~~~~~~~~~~~~~~~~~~~~~~ Axioms ~~~~~~~~~~~~~~~~~~~~~~~~~~" ;
  Loggers.print_newline log ;

  (* Construct MVBDU  *)
  (* let (p: c_path) = [] in
  let (_: c_path) = [(Bounded 0 (*x*), Bounded 0 (*x*))] in
  let m = m_and (p_agent p) (p_agent p) in *)

  (* Construct list of variables *)
  let v = Formula_G.variables_list_of_mvbdu m in

  (* Print *)
  Formula_G.print parameters m ;
  Formula_G.print_variables_list parameters v ;


  (*** (BONUS) STEP 3: Cloture by the external lemma outputed from static analysis ***)

  (* TODO: Apply rewriting rules defined in semantics section.
   * A proof should ensure that no matter the order of the application of rules on a formula we obtain the same fix point. *)
  
  (* To re-encode the logic inside a kappa graph we should have a canonical form:
   *
   * e -> structural axioms + axioms from static analysis
   *
   * World 1         | World 2
   * ----------------|--------------
   * (F1.e) + (F2.e) | (F1 + F2).e
   * (F1.e) . (F2.e) | (F1 . F2).e
   * not(F1.e) .e    | (not(F1) + not(e)).e <=> not(F1).e
   * /!\ This: ^^ solve the issue, not(F1) still in World 2 *)

  (* TODO: Get refinements lemma from static analysis (Remanent state) *)
  (* let _ = Remanent_state.get_internal_constraints_list state in *)


  (*** (MEGA BONUS) STEP 4: Reverse result of cloture using domain to output it to static analyser ***)

  (* To be continued... *)


  Loggers.fprintf log "------------------------------------------------------------" ;
  Loggers.print_newline log ;
  ()
