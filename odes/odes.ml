(** Network/ODE generation
  * Creation: 15/07/2016
  * Last modification: Time-stamp: <Feb 13 2017>
*)

let local_trace = false

let debug s =
  if local_trace || !Parameter.debugModeOn
  then Format.kfprintf (fun f -> Format.pp_print_break f 0 0)
      Format.err_formatter s
  else Format.ifprintf Format.err_formatter s

module Make(I:Ode_interface_sig.Interface) =
struct

  let alg_of_int i =
    Locality.dummy_annot (Alg_expr.CONST (Nbr.I i))

  let alg_of_float f =
    Locality.dummy_annot (Alg_expr.CONST (Nbr.F f))

  module SpeciesSetMap =
    SetMap.Make
      (struct
        type t = I.chemical_species
        let compare = compare
        let print = I.print_chemical_species ?compil:None
      end)
  module SpeciesSet = SpeciesSetMap.Set
  module SpeciesMap = SpeciesSetMap.Map

  type connected_component_id = int
  let fst_cc_id = 1
  let next_cc_id = succ

  module Store =
    SetMap.Make
      (struct
        type t =
          I.rule_id_with_mode * connected_component_id * I.connected_component
        let compare (a,b,c) (a',b',c') =
          let x = compare (a,b) (a',b') in
          if x <> 0 then x else I.compare_connected_component c c'
        let print a ((r,ar,dir),cc_id,cc) =
          let () =
            Format.fprintf a
              "Component_wise:(%a,%s,%s,%i,%a)"
              I.print_rule_id r
              (match ar with I.Usual -> "@" | I.Unary -> "(1)")
              (match dir with I.Direct -> "->" | I.Op -> "<-")
              cc_id
              (I.print_connected_component ?compil:None) cc
          in
          let () = I.print_rule_id a r in
          let () = Format.fprintf a "cc_id: %i \n" cc_id in
          I.print_connected_component a cc
      end)

  module StoreMap = Store.Map

  type id = int
  type ode_var_id = id
  (*  type intro_coef_id = id*)
  type var_id = id
  type obs_id = id
  type rule_id = id
  let fst_id = 1
  let next_id id = id + 1

  type ode_var =
    | Noccurrences of I.canonic_species
    | Nembed of I.canonic_species | Token of int | Dummy

  type lhs_decl = Init_decl | Var_decl of string | Init_value of ode_var

  module VarSetMap =
    SetMap.Make
      (struct
        type t = ode_var
        let compare = compare
        let print log x =
          match x with
          | Nembed x | Noccurrences x -> I.print_canonic_species log x
          | Token x -> Format.fprintf log "%i" x
          | Dummy -> ()
      end)
  module VarSet = VarSetMap.Set
  module VarMap = VarSetMap.Map

  type 'a decl =
    | Var of
        var_id * string option * ('a,int) Alg_expr.e Locality.annot
    | Init_expr of
        var_id  * ('a,int) Alg_expr.e Locality.annot * ode_var_id list
    | Dummy_decl

  let var_id_of_decl decl =
    match decl with
    | Var (a,_,_) -> a
    | Init_expr (a,_,_) -> a
    | Dummy_decl -> fst_id

  type enriched_rule =
    {
      comment: string;
      rule_id_with_mode: (rule_id*I.arity*I.direction);
      rule: I.rule ;
      lhs: I.pattern ;
      lhs_cc: (connected_component_id * I.connected_component) list ;
      divide_rate_by: int
    }

  let get_comment e = e.comment
  let get_rule_id_with_mode e = e.rule_id_with_mode
  let get_rule e = e.rule
  let get_lhs e = e.lhs
  let get_lhs_cc e = e.lhs_cc
  let get_divide_rate_by e = e.divide_rate_by

  let rule_id_of e =
    let (a,_,_) = e.rule_id_with_mode in a

  let arity_of e =
    let (_,a,_) = e.rule_id_with_mode in a

  let direction_of e =
    let (_,_,a) = e.rule_id_with_mode in a

  let var_of_rate (rule_id,arity,direction) =
    match arity,direction with
    | I.Usual,I.Direct -> Ode_loggers_sig.Rate rule_id
    | I.Unary,I.Direct -> Ode_loggers_sig.Rateun rule_id
    | I.Usual,I.Op -> Ode_loggers_sig.Rated rule_id
    | I.Unary,I.Op -> Ode_loggers_sig.Rateund rule_id

  let var_of_rule rule =
    var_of_rate rule.rule_id_with_mode

  type ('a,'b) network =
    {
      rules : enriched_rule list ;
      ode_variables : VarSet.t ;
      reactions:
        (id list * id list *
         (('a,'b) Alg_expr.e Locality.annot*id Locality.annot) list
         * enriched_rule) list ;

      ode_vars_tab: ode_var Mods.DynArray.t ;
      id_of_ode_var: ode_var_id VarMap.t ;
      fresh_ode_var_id: ode_var_id ;

      species_tab: (I.chemical_species*int) Mods.DynArray.t ;

      cc_cache: I.cache ;
      varmap: var_id Mods.IntMap.t ;
      tokenmap: ode_var_id Mods.IntMap.t ;

      fresh_var_id: var_id ;
      var_declaration: 'a decl list ;

      n_rules: int ;

      obs: (obs_id * ('a,'b) Alg_expr.e Locality.annot) list ;
      n_obs: int ;
      time_homogeneous_obs: bool option ;
      time_homogeneous_vars: bool option ;
      time_homogeneous_rates: bool option ;
    }

  let may_be_time_homogeneous_gen a =
    match
      a
    with
    | Some false -> false
    | Some true | None -> true

  let may_be_not_time_homogeneous_gen a =
      match
        a
      with
      | Some false | None -> true
      | Some true -> false

  let var_may_be_not_time_homogeneous network =
      may_be_not_time_homogeneous_gen network.time_homogeneous_vars

  let obs_may_be_not_time_homogeneous network =
    var_may_be_not_time_homogeneous network
    || may_be_not_time_homogeneous_gen network.time_homogeneous_obs

  let rate_may_be_not_time_homogeneous network =
    var_may_be_not_time_homogeneous network
    || may_be_not_time_homogeneous_gen network.time_homogeneous_rates

  let may_be_not_time_homogeneous network =
    rate_may_be_not_time_homogeneous network
    || may_be_not_time_homogeneous_gen network.time_homogeneous_obs

  let get_fresh_var_id network = network.fresh_var_id
  let get_last_var_id network = network.fresh_var_id-1
  let inc_fresh_var_id network =
    {network with fresh_var_id = next_id network.fresh_var_id}
  let get_fresh_ode_var_id network = network.fresh_ode_var_id
  let get_last_ode_var_id network = network.fresh_ode_var_id-1
  let inc_fresh_ode_var_id network =
    {network with fresh_ode_var_id = next_id network.fresh_ode_var_id}
  let get_fresh_obs_id network = network.n_obs
  let last_fresh_obs_id network = network.n_obs-1
  let inc_fresh_obs_id network =
    {network with n_obs = next_id network.n_obs}


  let fold_left_swap f a b =
    List.fold_left
      (fun a b -> f b a)
      b a

  let get_compil = I.get_compil

  let init compil =
    {
      rules = [] ;
      reactions = [] ;
      ode_variables = VarSet.empty ;
      ode_vars_tab = Mods.DynArray.create 0 Dummy ;
      id_of_ode_var = VarMap.empty ;
      species_tab = Mods.DynArray.create 0
          (I.dummy_chemical_species compil,1) ;
      cc_cache = I.empty_cache compil ;
      fresh_ode_var_id = fst_id ;
      fresh_var_id = fst_id ;
      varmap = Mods.IntMap.empty ;
      tokenmap = Mods.IntMap.empty ;
      var_declaration = [];
      n_rules = 0 ;
      obs = [] ;
      n_obs = 1 ;
      time_homogeneous_vars = None ;
      time_homogeneous_obs = None ;
      time_homogeneous_rates = None  }

  let from_nembed_correct compil nauto =
    if I.do_we_count_in_embeddings compil then
      Ode_loggers.Nil
    else
      Ode_loggers.Div nauto

  let from_nocc_correct compil nauto =
    if I.do_we_count_in_embeddings compil then
      Ode_loggers.Mul nauto
    else
      Ode_loggers.Nil

  let to_nembed_correct compil nauto =
    if I.do_we_count_in_embeddings compil then
      Ode_loggers.Nil
    else
      Ode_loggers.Mul nauto

  let to_nocc_correct compil nauto =
    if I.do_we_count_in_embeddings compil then
      Ode_loggers.Div nauto
    else
      Ode_loggers.Nil

  let lift f compil expr nauto =
    match f compil nauto with
    | Ode_loggers.Nil -> expr
    | Ode_loggers.Div n ->
      Alg_expr.BIN_ALG_OP
        (Operator.DIV,Locality.dummy_annot expr,alg_of_int n)
    | Ode_loggers.Mul n ->
      Alg_expr.BIN_ALG_OP
        (Operator.MULT,alg_of_int n,Locality.dummy_annot expr)

  let to_nembed = lift to_nembed_correct
  let to_nocc = lift to_nocc_correct
  let from_nembed = lift from_nembed_correct
  let from_nocc = lift from_nocc_correct

  let is_known_variable variable network =
    VarSet.mem variable network.ode_variables

  let add_new_var var network =
    let () =
      Mods.DynArray.set
        network.ode_vars_tab
        (get_fresh_ode_var_id network)
        var
    in
    let network =
      { network
        with
          ode_variables = VarSet.add var network.ode_variables ;
          id_of_ode_var = VarMap.add var network.fresh_ode_var_id network.id_of_ode_var ;
      }
    in
    inc_fresh_ode_var_id network,
    get_fresh_ode_var_id network

  let add_new_canonic_species canonic species network =
    let () =
      Mods.DynArray.set
        network.species_tab
        (get_fresh_ode_var_id network)
        (species, I.nbr_automorphisms_in_chemical_species species)
    in
    add_new_var (Nembed canonic) network

  let add_new_token token network =
    let network, id = add_new_var (Token token) network in
    {network with tokenmap = Mods.IntMap.add token id network.tokenmap},
    id

  let enrich_rule cache compil rule rule_id_with_mode =
    let lhs = I.lhs compil rule_id_with_mode rule in
    let _,lhs_cc =
      List.fold_left
        (fun (counter,list) cc ->
           (next_cc_id counter,
            (counter,cc)::list))
        (fst_cc_id,[])
        (List.rev (I.connected_components_of_patterns lhs))
    in
    let cache, divide_rate_by =
        I.divide_rule_rate_by cache compil rule
    in
    cache,
    {
      comment = I.rate_name compil rule rule_id_with_mode ;
      rule_id_with_mode = rule_id_with_mode ;
      rule = rule ;
      lhs = lhs ;
      lhs_cc = lhs_cc ;
      divide_rate_by = divide_rate_by ;
    }

  let add_embedding key embed store =
    let old_list =
      StoreMap.find_default [] key store
    in
    StoreMap.add key (embed::old_list) store

  let add_embedding_list key lembed store =
    let old_list =
      StoreMap.find_default [] key store
    in
    let new_list =
      fold_left_swap (fun a b -> a::b)
        lembed
        old_list
    in
    StoreMap.add key new_list store

  let translate_canonic_species compil canonic species remanent =
    let id_opt =
      VarMap.find_option
        (Nembed canonic)
        (snd remanent).id_of_ode_var in
    match
      id_opt
    with
    | None ->
      let () = debug "A NEW SPECIES IS DISCOVERED @." in
      let () = debug "canonic form: %a@."
          (I.print_canonic_species ~compil) canonic in
      let () = debug "species: %a@.@."
          (I.print_chemical_species ~compil) species in
      let to_be_visited, network = remanent in
      let network, id = add_new_canonic_species canonic species network
      in
      (species::to_be_visited,network), id
    | Some i ->
      let () = debug "ALREADY SEEN SPECIES @." in
      let () = debug "canonic form: %a@."
          (I.print_canonic_species ~compil) canonic in
      let () = debug "species: %a@.@."
          (I.print_chemical_species ~compil) species in
      remanent,i

  let translate_species compil species remanent =
    translate_canonic_species compil
      (I.canonic_form species) species remanent

  let translate_token token remanent =
    let id_opt =
      VarMap.find_option
        (Token token) (snd remanent).id_of_ode_var
    in
    match id_opt with
    | None ->
      let to_be_visited, network = remanent in
      let network, id = add_new_token token network in
      (to_be_visited, network), id
    | Some i -> remanent, i

  let petrify_species compil species =
    translate_canonic_species compil
      (I.canonic_form species) species

  let petrify_species_list compil l remanent =
    fold_left_swap
      (fun species (remanent,l) ->
         let remanent, i =
           petrify_species compil species remanent
         in
         remanent,(i::l))
      l
      (remanent,[])

  let petrify_mixture compil mixture (acc,network) =
    let cc_cache',species =
      I.connected_components_of_mixture
        compil network.cc_cache mixture in
    petrify_species_list compil species (acc,{network with cc_cache = cc_cache'})

  let add_to_prefix_list connected_component key prefix_list store acc =
    let list_embeddings =
      StoreMap.find_default [] key store
    in
    List.fold_left
      (fun new_list prefix ->
         List.fold_left
           (fun new_list (embedding,chemical_species) ->
              ((connected_component,embedding,chemical_species)::prefix)::new_list)
           new_list
           list_embeddings
      )
      acc prefix_list

  let nembed_of_connected_component compil network connected_component =
    VarMap.fold
      (fun vars id alg ->
         match vars with
         | Token _ | Dummy -> alg
         | Nembed _ | Noccurrences _ ->
           let from =
             match vars with
             | Token _ | Dummy -> assert false
             | Nembed _ -> from_nembed compil
             | Noccurrences _ -> from_nocc compil
           in
           let (species,nauto) = Mods.DynArray.get network.species_tab id in
           let n_embs =
             List.length
               (I.find_embeddings compil connected_component species) in
           if n_embs = 0 then alg
           else
             let species = Alg_expr.KAPPA_INSTANCE id in
             let term =
               if n_embs = 1 then species
               else
                 to_nembed compil
                   (from
                      (Alg_expr.BIN_ALG_OP
                         (Operator.MULT,
                          alg_of_int n_embs,
                          Locality.dummy_annot species)) nauto)
                   nauto
             in
             if alg = Alg_expr.CONST (Nbr.zero) then term
             else
               Alg_expr.BIN_ALG_OP
                 (Operator.SUM,
                  Locality.dummy_annot alg,
                  Locality.dummy_annot term)
      )
      network.id_of_ode_var
      (Alg_expr.CONST (Nbr.zero))

  let rec convert_alg_expr compil network alg =
    match
      alg
    with
    | Alg_expr.BIN_ALG_OP (op, arg1, arg2 ),loc ->
      Alg_expr.BIN_ALG_OP
        (op, convert_alg_expr compil network arg1,
         convert_alg_expr compil network arg2),loc
    | Alg_expr.UN_ALG_OP (op, arg),loc ->
      Alg_expr.UN_ALG_OP
        (op, convert_alg_expr compil network arg),loc
    | Alg_expr.KAPPA_INSTANCE cc, loc ->
      begin
        let f x =
          Array.fold_left
            (fun expr h ->
               Alg_expr.BIN_ALG_OP
                 (Operator.MULT,
                  Locality.dummy_annot expr,
                  Locality.dummy_annot
                    (nembed_of_connected_component compil network  h)))
            (Alg_expr.CONST Nbr.one)
            x
        in
        match cc with
        | [] -> alg_of_int 0
        | head::tail ->
          List.fold_left
            (fun acc l ->
               Alg_expr.BIN_ALG_OP
                 (Operator.SUM,
                  Locality.dummy_annot acc,
                  Locality.dummy_annot @@
                  f l))
            (f head)
            tail, loc
      end
    | (Alg_expr.TOKEN_ID _ | Alg_expr.ALG_VAR _ | Alg_expr.CONST _
      |Alg_expr.STATE_ALG_OP _),_ as a -> a
    | Alg_expr.IF (cond,yes,no),pos ->
      Alg_expr.IF (convert_bool_expr compil network cond,
                   convert_alg_expr compil network yes,
                   convert_alg_expr compil network no),pos

  and convert_bool_expr compil network = function
    | (Alg_expr.TRUE | Alg_expr.FALSE),_ as a -> a
    | Alg_expr.COMPARE_OP (op,a,b),pos ->
      Alg_expr.COMPARE_OP (op,
                           convert_alg_expr compil network a,
                           convert_alg_expr compil network b),pos
    | Alg_expr.BOOL_OP (op,a,b),pos ->
      Alg_expr.BOOL_OP (op,
                        convert_bool_expr compil network a,
                        convert_bool_expr compil network b),pos
  let add_reaction
      compil enriched_rule embedding_forest mixture remanent =
    let rule = enriched_rule.rule in
    let _  = debug "REACTANTS\n" in
    let remanent, reactants =
      petrify_mixture compil mixture remanent in
    let _  = debug "PRODUCT\n" in
    let products = I.apply compil rule embedding_forest mixture in
    let tokens = I.token_vector rule in
    let remanent, products =
      petrify_mixture compil products remanent in
    let remanent, tokens =
      List.fold_left
        (fun (remanent, tokens) (a,b) ->
           let remanent, id = translate_token b remanent in
           let a' = convert_alg_expr compil (snd remanent) a in
           remanent,(a',(Locality.dummy_annot id))::tokens)
        (remanent,[])
        tokens
    in
    let to_be_visited, network = remanent in
    let network =
      {
        network with
        reactions =
          (List.rev reactants, List.rev products, List.rev tokens,
           enriched_rule)::network.reactions
      }
    in
    to_be_visited, network

  let initial_network compil network initial_states rules =
    List.fold_left
      (fun remanent enriched_rule ->
         match enriched_rule.lhs_cc with
         | [] ->
           begin
             let _,embed,mixture = I.disjoint_union compil [] in
             let () = debug "add new reaction" in
             add_reaction compil enriched_rule embed mixture remanent
           end
         | _::_ -> remanent
      )
      (List.fold_left
         (fun remanent species -> fst (translate_species compil species remanent))
         ([],network)
         initial_states) rules

  let compute_reactions compil network rules initial_states =
    (* Let us annotate the rules with cc decomposition *)
    let n_rules = List.length rules in
    let cache = I.empty_lkappa_cache () in
    let rules =
      List.rev
        (snd
           (List.fold_left
              (fun ((id, cache), list) rule ->
                 let modes = I.valid_modes compil rule id in
                 List.fold_left
                   (fun ((id,cache), list) mode ->
                      let cache, elt = enrich_rule cache compil rule mode in
                      (id,cache), elt::list)
                   ((next_id id,cache),list) modes)
              ((fst_id,cache),[]) (List.rev rules)))
    in
    let to_be_visited, network =
      initial_network
        compil network initial_states rules
    in
    let network =
      {network
       with n_rules = n_rules;
            rules = rules }
    in
    let store = StoreMap.empty in
    (* store maps each cc in the lhs of a rule to the list of embedding between this cc and a pattern in set\to_be_visited *)
    let rec aux to_be_visited network store =
      match
        to_be_visited
      with
      | []   -> network

      | new_species::to_be_visited ->
        let () = debug "@[<v 2>@[test for the new species:@ %a@]"
            (I.print_chemical_species ~compil) new_species in
        (* add in store the embeddings from cc of lhs to new_species,
           for unary application of binary rule, the dictionary of species is updated, and the reaction entered directly *)
        let store, to_be_visited, network  =
          List.fold_left
            (fun
              (store_old_embeddings, to_be_visited, network)  enriched_rule ->
              (* regular application of tules, we store the embeddings*)
              let () = debug "@[<v 2>test for rule %i (Aut:%i)@[%a@]"
                  (rule_id_of enriched_rule)
                  enriched_rule.divide_rate_by
                  (I.print_rule ~compil) enriched_rule.rule in
              match arity_of enriched_rule  with
              | I.Usual ->
                begin
                  let () = debug "regular case" in
                  let store_new_embeddings =
                    List.fold_left
                      (fun store (cc_id,cc) ->
                         let () = debug "find embeddings" in
                         let lembed = I.find_embeddings compil cc new_species in
                         add_embedding_list
                           (enriched_rule.rule_id_with_mode,cc_id,cc)
                           (List.rev_map (fun a -> a,new_species) (List.rev lembed))
                           store
                      )
                      StoreMap.empty
                      enriched_rule.lhs_cc
                  in
                  let (),store_all_embeddings =
                    StoreMap.map2_with_logs
                      (fun _ a _ _ _ -> a)
                      ()
                      ()
                      (fun _ _ b -> (),b)
                      (fun _ _ b -> (),b)
                      (fun _ _ b c ->
                         (),List.fold_left
                           (fun list elt -> elt::list)
                           b c)
                      store_old_embeddings
                      store_new_embeddings
                  in
                  (* compute the embedding betwen lhs and tuple of species that contain at least one occurence of new_species *)
                  let dump_store store =
                    if local_trace || !Parameter.debugModeOn
                    then
                      StoreMap.iter
                        (fun ((a,ar,dir),id,b) c ->
                           let () = debug "@[<v 2>* rule:%i %s %s  cc:%i:@[%a@]:" a
                               (match ar with I.Usual -> "@"
                                            | I.Unary -> "(1)")
                               (match dir with I.Direct -> "->"
                                             | I.Op -> "<-")
                               id
                               (I.print_connected_component ~compil) b
                           in
                           let () =
                             List.iter (fun (_,b) -> debug "%a"
                                           (I.print_chemical_species ~compil) b) c
                           in
                           let () = debug "@]" in
                           ()
                        )
                        store
                  in
                  let () = debug "new embeddings" in
                  let () = dump_store   store_new_embeddings in
                  (*  let () = debug "old embeddings" in
                      let () = dump_store   store_old_embeddings in

                      let () = debug "all embeddings" in
                      let () = dump_store   store_all_embeddings in*)

                  let _,new_embedding_list =
                    List.fold_left
                      (fun (partial_emb_list,partial_emb_list_with_new_species) (cc_id,cc) ->
                         (* First case, we complete with an embedding towards the new_species *)
                         let label =
                           enriched_rule.rule_id_with_mode,cc_id,cc
                         in
                         let partial_emb_list_with_new_species =
                           add_to_prefix_list cc label   partial_emb_list
                             store_new_embeddings
                             (add_to_prefix_list cc label partial_emb_list_with_new_species
                                store_all_embeddings [])
                         in
                         let partial_emb_list =
                           add_to_prefix_list cc
                             label partial_emb_list store_old_embeddings []
                         in

                         partial_emb_list, partial_emb_list_with_new_species
                      )
                      ([[]],[])
                      enriched_rule.lhs_cc
                  in
                  (* compute the corresponding rhs, and put the new species in the working list, and store the corrsponding reactions *)
                  let to_be_visited, network =
                    List.fold_left
                      (fun remanent list ->
                         let () = debug "compute one refinement" in
                         let () = debug "disjoint union @[<v>%a@]"
                             (Pp.list Pp.space (fun f (_,_,s) ->
                                  I.print_chemical_species ~compil f s))
                             list
                         in
                         let _,embed,mixture = I.disjoint_union compil list in
                         let () = debug "add new reaction" in
                         add_reaction
                           compil enriched_rule embed mixture remanent)
                      (to_be_visited,network)
                      new_embedding_list
                  in
                  let () = debug "@]" in
                  store_all_embeddings,to_be_visited,network
                end

              | I.Unary ->
                begin
                  (* unary application of binary rules *)
                  let () = debug "unary case" in
                  let to_be_visited, network =
                    let lembed =
                      I.find_embeddings_unary_binary compil enriched_rule.lhs new_species in
                    fold_left_swap
                      (fun embed ->
                         add_reaction
                           compil  enriched_rule embed
                           (I.lift_species compil new_species))
                      lembed
                      (to_be_visited, network)
                  in
                  let () = debug "@]" in
                  store_old_embeddings, to_be_visited, network
                end
            )
            (store, to_be_visited, network)
            rules
        in
        let () = debug "@]" in
        aux to_be_visited network store
    in
    let o = aux to_be_visited network store in
    let () = debug "@]@." in
    o

  let convert_tokens compil network =
    Tools.recti
      (fun network a ->
         snd (fst (translate_token a ([],network))))
      network
      (I.nb_tokens compil)

  let species_of_species_id network =
    (fun i -> Mods.DynArray.get network.species_tab i)

  let get_reactions network = network.reactions

  let convert_initial_state compil intro network =
    let b,c,a = intro in
    convert_alg_expr compil network (b,a) ,
    match I.token_vector_of_init c with
    | [] ->
      let m = I.mixture_of_init compil c in
      let cc_cache',cc =
        I.connected_components_of_mixture compil network.cc_cache m
      in
      let network = {network with cc_cache = cc_cache'} in
      List.fold_left
        (fun (network,acc) x ->
           let (_,n'),v = translate_species compil x ([],network) in n',v::acc)
        (network,[]) (List.rev cc)
    | l ->
      List.fold_right (fun (_,token) (network,acc) ->
          let (_,n'),v = translate_token token ([],network) in n',v::acc) l (network,[])

  let translate_token token network =
    snd (translate_token token ([],network))

  let convert_var_def compil network variable_def =
    let a,b = variable_def in
    a,convert_alg_expr compil network b

  let convert_var_defs compil network =
    let list_var = I.get_variables compil in
    let init = I.get_init compil in
    let list, network =
      Tools.array_fold_lefti
        (fun i (list,network) def ->
           let a,b = convert_var_def compil network def in
           (Var (get_fresh_var_id network,Some a,b))::list,
           inc_fresh_var_id
             {network with
              varmap =
                Mods.IntMap.add i (get_fresh_var_id network) network.varmap})
        ([],network)
        list_var
    in
    let init_tab =
      Mods.DynArray.make (get_fresh_ode_var_id network) []
    in
    let add i j =
      Mods.DynArray.set
        init_tab
        i
        (j::(Mods.DynArray.get init_tab i))
    in
    let list, network =
      List.fold_left
        (fun (list,network) def ->
           let b,(network,c) =
             convert_initial_state compil def network in
           let () =
             List.iter
               (fun id -> add id (get_fresh_var_id network))
               c
           in
           (Init_expr (network.fresh_var_id,b,c))::list,
           (inc_fresh_var_id network)
        )
        (list,network)
        init
    in
    let size = List.length list in
    let npred =
      Mods.DynArray.create (get_fresh_var_id network) 0
    in
    let lsucc =
      Mods.DynArray.create (get_fresh_var_id network) []
    in
    let dec_tab =
      Mods.DynArray.create network.fresh_var_id
        (Dummy_decl,None,Locality.dummy_annot (Alg_expr.CONST Nbr.zero))
    in
    let add_succ i j =
      let () = Mods.DynArray.set npred j (1+(Mods.DynArray.get npred j)) in
      let () = Mods.DynArray.set lsucc i (j::(Mods.DynArray.get lsucc i)) in
      ()
    in
    let rec aux_alg id expr =
      match expr with
      | Alg_expr.CONST _,_ -> ()
      | Alg_expr.BIN_ALG_OP (_,a,b),_ -> (aux_alg id a;aux_alg id b)
      | Alg_expr.UN_ALG_OP (_,a),_ -> aux_alg id a
      | Alg_expr.STATE_ALG_OP _,_ -> ()
      | Alg_expr.IF (cond,yes,no),_ ->
        aux_bool id cond; aux_alg id yes; aux_alg id no
      | Alg_expr.TOKEN_ID s,_ ->
        let id' = translate_token s network in
        let list = Mods.DynArray.get init_tab id' in
        List.iter (fun id'' -> add_succ id id'') list
      | Alg_expr.KAPPA_INSTANCE id',_ ->
        let list = Mods.DynArray.get init_tab id' in
        List.iter (fun id'' -> add_succ id id'') list
      | Alg_expr.ALG_VAR id',_ ->
        let id_opt = Mods.IntMap.find_option id' network.varmap in
        match id_opt with
        | Some id'' -> add_succ id id''
        | None -> ()
    and aux_bool id = function
      | (Alg_expr.TRUE | Alg_expr.FALSE),_ -> ()
      | Alg_expr.COMPARE_OP (_,a,b),_ ->
        aux_alg id a; aux_alg id b
      | Alg_expr.BOOL_OP (_,a,b),_ ->
        aux_bool id a; aux_bool id b
    in
    let () =
      List.iter
        (fun decl ->
           match decl
           with
           | Dummy_decl -> ()
           | Init_expr (id,b,_) ->
             let () = Mods.DynArray.set dec_tab id (decl,None,b)
             in aux_alg id b
           | Var (id,a,b) ->
             let () = Mods.DynArray.set dec_tab id (decl,a,b) in
             aux_alg id b) list in
    let top_sort =
      let clean k to_be_visited =
        let l = Mods.DynArray.get lsucc k in
        List.fold_left
          (fun to_be_visited j ->
             let old = Mods.DynArray.get npred j in
             let () = Mods.DynArray.set npred j (old-1) in
             if old = 1 then j::to_be_visited else to_be_visited)
          to_be_visited l
      in
      let to_be_visited =
        let rec aux k l =
          if k < fst_id
          then l
          else
          if Mods.DynArray.get npred k = 0
          then
            aux (k-1) (k::l)
          else
            aux (k-1) l
        in
        aux (network.fresh_var_id-1) []
      in
      let rec aux to_be_visited l =
        match to_be_visited with
        | [] -> List.rev l
        | h::t -> aux (clean h t) (h::l)
      in
      let l = aux to_be_visited [] in
      let l =
        List.rev_map
          (fun x ->
             let decl,_,_ = Mods.DynArray.get dec_tab x in decl
          ) l
      in l
    in
    let size' = List.length top_sort in
    if size' = size
    then
      {network with var_declaration = top_sort}
    else
      let () = Printf.fprintf stdout "Circular dependencies\n" in
      assert false

  let convert_one_obs obs network =
    let a,b = obs in
    a,convert_alg_expr b network

  let convert_obs compil network =
    let list_obs = I.get_obs compil in
    let network =
      List.fold_left
        (fun network obs ->
           inc_fresh_obs_id
             {network with
              obs = (get_fresh_obs_id network,
                     convert_alg_expr compil network (Locality.dummy_annot obs))
                    ::network.obs})
        network
        list_obs
    in
    {network with
     obs = List.rev network.obs;
     n_obs = network.n_obs - 1}

  let species_of_initial_state compil cc_cache =
    List.fold_left
      (fun (cc_cache,list) (_,r,_) ->
         let b = I.mixture_of_init compil r in
         let cc_cache',acc =
           I.connected_components_of_mixture compil cc_cache b
         in
         cc_cache',List.rev_append acc list)
      (cc_cache,[])

  type ('a,'b) rate = ('a,'b) Alg_expr.e Locality.annot

  type ('a,'b) sort_rules_and_decl =
    {
      const_decl_set : Mods.StringSet.t ;
      const_decl: 'a decl list ;
      var_decl: 'a decl list ;
      init: 'a decl list ;
      const_rate :
        (enriched_rule * ('a,'b) rate) list ;
      var_rate :
        (enriched_rule * ('a,'b) rate) list ;
    }

  let init_sort_rules_and_decl =
    {
      const_decl_set = Mods.StringSet.empty ;
      const_decl = [] ;
      var_decl = [] ;
      const_rate = [] ;
      var_rate = [] ;
      init = [] ;
    }

  let split_var_declaration network sort_rules_and_decls =
    let decl =
      List.fold_left
        (fun sort_decls decl ->
           match decl with
           | Dummy_decl
           | Var (_,None,_)
           | Init_expr _ ->
             {
               sort_decls
               with
                 init = decl::sort_decls.init}
           | Var (_id,Some a,b) ->
             if Ode_loggers_sig.is_expr_const b
             then
               {
                 sort_decls
                 with
                   const_decl_set = Mods.StringSet.add a sort_decls.const_decl_set ;
                   const_decl = decl::sort_decls.const_decl
               }
             else
               {
                 sort_decls
                 with
                   var_decl =
                     decl::sort_decls.var_decl
               })
        sort_rules_and_decls
        network.var_declaration
    in
    {decl
     with
      const_decl = List.rev decl.const_decl ;
      var_decl = List.rev decl.var_decl ;
      init = List.rev decl.init}

  let split_rules compil network sort_rules_and_decls =
    let sort =
      List.fold_left
        (fun sort_rules enriched_rule ->
           let rate =
             I.rate
               compil enriched_rule.rule enriched_rule.rule_id_with_mode
           in
           match rate with
           | None -> sort_rules
           | Some rate ->
             let rate = convert_alg_expr compil network rate in
             let sort_rules =
               if Ode_loggers_sig.is_expr_const rate
               then
                 {
                   sort_rules
                   with const_rate =
                          (enriched_rule,
                           rate)::sort_rules.const_rate
                 }
               else
                 {
                   sort_rules
                   with var_rate =
                          (enriched_rule,
                           rate)::sort_rules.var_rate
                 }
             in
             sort_rules)
        sort_rules_and_decls
        network.rules
    in
    {sort
     with const_rate = List.rev sort.const_rate ;
          var_rate = List.rev sort.var_rate}

  let split_rules_and_decl compil network =
    split_rules compil network (split_var_declaration network init_sort_rules_and_decl)

  let time_homogeneity_of_rates compil network =
    let rules = network.rules in
    List.for_all
      (fun rule ->
        let rate_opt = I.rate compil rule.rule rule.rule_id_with_mode in
        match rate_opt with
        | None -> true
        | Some rate -> Ode_loggers_sig.is_expr_time_homogeneous rate)
      rules

  let time_homogeneity_of_vars network =
    let vars_decl = network.var_declaration in
    List.for_all
      (fun decl ->
         match decl with
         | Dummy_decl | Init_expr _ -> true
         | Var (_,_,expr) -> Ode_loggers_sig.is_expr_time_homogeneous expr)
      vars_decl

  let time_homogeneity_of_obs network =
    let obs = network.obs in
    List.for_all
      (fun (_,expr) -> Ode_loggers_sig.is_expr_time_homogeneous expr)
      obs

  let check_time_homogeneity ~ignore_obs compil network =
    {network
     with
      time_homogeneous_vars = Some (time_homogeneity_of_vars network) ;
      time_homogeneous_obs =
        Some (
          if ignore_obs then true
          else
            time_homogeneity_of_obs network) ;
      time_homogeneous_rates = Some (time_homogeneity_of_rates compil network) }

  let string_of_bool b =
    if b then "true" else "false"

  let string_of_bool_opt b_opt =
    match b_opt with
    | None -> "none"
    | Some b -> string_of_bool b

  let network_from_compil ~ignore_obs compil =
    let () = Format.printf "+ generate the network... @." in
    let rules = I.get_rules compil in
    let network = init compil in
    let () = Format.printf "\t -initial states @." in
    let cc_cache,initial_state =
      species_of_initial_state compil network.cc_cache
        (I.get_init compil)
    in
    let network = {network with cc_cache = cc_cache} in
    let () = Format.printf "\t -saturating the set of molecular species @." in
    let network =
      compute_reactions compil network rules initial_state in
    let () = Format.printf "\t -tokens @." in
    let network = convert_tokens compil network in
    let () = Format.printf "\t -variables @." in
    let network = convert_var_defs compil network in
    let () = Format.printf "\t -observables @." in
    let network = convert_obs compil network in
    let () = Format.printf "\t -check time homogeneity @." in
    let network = check_time_homogeneity ~ignore_obs compil network in
    network

  let handler_init =
    {
      Network_handler.int_of_obs = (fun i  -> i) ;
      Network_handler.int_of_kappa_instance = (fun i -> i) ;
      Network_handler.int_of_token_id = (fun i ->  i) ;
    }

  let handler_expr network =
    {
      Network_handler.int_of_obs =
        (fun s -> Mods.IntMap.find_default s s network.varmap) ;
      Network_handler.int_of_kappa_instance = (fun i -> i) ;
      Network_handler.int_of_token_id =
        (fun s -> Mods.IntMap.find_default 0 s network.tokenmap) ;
    }

  let increment
      is_zero ?init_mode:(init_mode=false) ?comment:(comment="") string_of_var logger logger_buffer x =
    if is_zero x
    then
      Ode_loggers.associate ~init_mode ~comment string_of_var logger logger_buffer  (Ode_loggers_sig.Init x)
    else
      Ode_loggers.increment ~init_mode ~comment string_of_var logger (Ode_loggers_sig.Init x)

  let affect_var is_zero ?init_mode:(init_mode=false) logger logger_buffer compil network decl =
    let handler_expr = handler_expr network in
    match decl with
    | Dummy_decl -> ()
    | Init_expr (id', expr, list) ->
      begin
        match list with
        | [] -> ()
        | [a] ->
          let species, n = species_of_species_id network a in
          let expr =
            Locality.dummy_annot
              (to_nembed compil (from_nocc compil (fst expr) n) n)
          in
          let comment =
            Format.asprintf "%a"
              (fun log  ->
                 I.print_chemical_species ~compil log )
              species
          in
          increment
            is_zero ~init_mode ~comment
            (I.string_of_var_id ~compil) logger logger_buffer a expr handler_expr
        | _ ->
          let () = Ode_loggers.associate
              ~init_mode
              (I.string_of_var_id ~compil)
              logger logger_buffer (Ode_loggers_sig.Expr id') expr handler_expr
          in
          List.iter
            (fun id ->
               let n =
                 snd (species_of_species_id network id)
               in
               let expr =
                 Locality.dummy_annot
                   (to_nembed compil
                      (from_nocc compil (Alg_expr.ALG_VAR id') n) n)
               in
               increment
                 is_zero (I.string_of_var_id ~compil)
                 logger logger_buffer ~init_mode id expr handler_init)
            list
      end
    | Var (id,_comment,expr) ->
      Ode_loggers.associate
        ~init_mode (I.string_of_var_id ~compil) logger logger_buffer
        (Ode_loggers_sig.Expr id) expr handler_expr

  let fresh_is_zero network =
    let is_zero = Mods.DynArray.create (get_fresh_ode_var_id network) true in
    let is_zero x =
      if Mods.DynArray.get is_zero x
      then
        let () = Mods.DynArray.set is_zero x false in
        true
      else
        false
    in is_zero

  let declare_rates_global logger network =
    let do_it f =
      Ode_loggers.declare_global logger (f network.n_rules)
    in
    let () = do_it (fun x -> Ode_loggers_sig.Rate x) in
    let () = do_it (fun x -> Ode_loggers_sig.Rated x) in
    let () = do_it (fun x -> Ode_loggers_sig.Rateun x) in
    let () = do_it (fun x -> Ode_loggers_sig.Rateund x) in
    let () =
      match Loggers.get_encoding_format logger with
      | Loggers.Octave | Loggers.Matlab ->
        Ode_loggers.print_newline logger
      | Loggers.Maple | Loggers.SBML | Loggers.TXT
      | Loggers.TXT_Tabular | Loggers.XLS
      | Loggers.Matrix | Loggers.DOT | Loggers.HTML | Loggers.HTML_Graph | Loggers.HTML_Tabular
      | Loggers.Json -> ()
    in
    ()

  let breakline = true

  let export_main
      ~command_line ~command_line_quotes ~data_file ~init_t ~max_t ~plot_period
      logger logger_buffer compil network split =
    let is_zero = fresh_is_zero network in
    let handler_expr = handler_expr network in
    let () = Ode_loggers.open_procedure logger "main" "main" [] in
    let command_line_closure logger =
      let () = Ode_loggers.print_comment ~breakline logger "command line: " in
      let () = Ode_loggers.print_comment ~breakline logger ("     "^command_line_quotes) in ()
    in
    let count = I.what_do_we_count compil in
    let rate_convention = I.rate_convention compil in
    let may_be_not_time_homogeneous = may_be_not_time_homogeneous network in
    let () =
      Ode_loggers.print_ode_preamble ~may_be_not_time_homogeneous
        ~count ~rate_convention logger command_line_closure ()
    in
    let () = Ode_loggers.print_newline logger in
    let () = Sbml_backend.open_box logger_buffer "listOfParameters" in
    let () = Sbml_backend.line_sbml logger_buffer in
    let () =
      Ode_loggers.associate (I.string_of_var_id ~compil) logger logger_buffer Ode_loggers_sig.Tinit (alg_of_float init_t) handler_expr in
    let () =
      Ode_loggers.associate (I.string_of_var_id ~compil) logger logger_buffer Ode_loggers_sig.Tend
        (alg_of_float max_t)
        handler_expr
    in
    let () =
      Ode_loggers.associate
        (I.string_of_var_id ~compil)
        logger logger_buffer Ode_loggers_sig.InitialStep
        (alg_of_float  0.000001) handler_expr
    in
    let () =
      Ode_loggers.associate
        (I.string_of_var_id ~compil)
        logger logger_buffer Ode_loggers_sig.Period_t_points
        (alg_of_float plot_period) handler_expr
    in
    let () = Ode_loggers.print_newline logger_buffer in
    let () =
      Ode_loggers.declare_global logger_buffer Ode_loggers_sig.N_ode_var
    in
    let () =
      Ode_loggers.associate
        (I.string_of_var_id ~compil)
        logger logger_buffer
        Ode_loggers_sig.N_ode_var
        (alg_of_int (get_last_ode_var_id network))
        handler_expr
    in
    let () =
      Ode_loggers.associate
        (I.string_of_var_id ~compil)
        logger logger_buffer
        Ode_loggers_sig.N_var
        (alg_of_int (get_last_var_id network))
        handler_expr
    in
    let () =
      Ode_loggers.associate
        (I.string_of_var_id ~compil)
        logger logger_buffer
        Ode_loggers_sig.N_obs
        (alg_of_int network.n_obs)
        handler_expr in
    let () =
      Ode_loggers.associate
        (I.string_of_var_id ~compil)
        logger logger_buffer
        Ode_loggers_sig.N_rules
        (alg_of_int network.n_rules)
        handler_expr
    in
    let () = Ode_loggers.print_newline logger_buffer in
    let () = Ode_loggers.declare_global logger_buffer (Ode_loggers_sig.Expr network.fresh_var_id) in
    let () = Ode_loggers.initialize logger_buffer (Ode_loggers_sig.Expr network.fresh_var_id) in
    let () = Ode_loggers.declare_global logger_buffer (Ode_loggers_sig.Init network.fresh_ode_var_id) in
    let () = Ode_loggers.initialize logger_buffer (Ode_loggers_sig.Init network.fresh_ode_var_id) in
    let () = Ode_loggers.print_newline logger_buffer in
    let () = Ode_loggers.start_time logger_buffer init_t in
    let () = Ode_loggers.print_newline logger_buffer in
    let () =
      if may_be_not_time_homogeneous
      then
        let () =
          Ode_loggers.associate
            (I.string_of_var_id ~compil)
            logger logger_buffer
            (Ode_loggers_sig.Init (get_last_ode_var_id network))
            (Locality.dummy_annot (Alg_expr.STATE_ALG_OP Operator.TIME_VAR))
            handler_init
        in
          Sbml_backend.do_sbml logger
            (fun logger ->
               Sbml_backend.print_parameters I.string_of_var_id
                 logger logger_buffer
                 Ode_loggers_sig.Time_scale_factor Nbr.one
            )
      else ()
    in
    let () =
      List.iter
        (affect_var is_zero logger logger_buffer ~init_mode:true compil network)
        network.var_declaration
    in
    let () = Ode_loggers.print_newline logger_buffer in
    let () = declare_rates_global logger_buffer network in
    let () =
      List.iter
        (fun (rule,rate) ->
           Ode_loggers.associate
             (I.string_of_var_id ~compil)
             ~comment:rule.comment logger logger_buffer
             (var_of_rate rule.rule_id_with_mode) rate handler_expr)
        split.const_rate
    in
    let () = Sbml_backend.close_box logger_buffer  "listOfParameters" in
    let titles = I.get_obs_titles compil in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_license_check logger in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_options logger in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_integrate logger in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.associate_nrows logger in
    let () = Ode_loggers.initialize logger Ode_loggers_sig.Tmp  in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_interpolate logger in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_dump_plots ~data_file ~command_line ~titles logger in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.close_procedure logger in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_newline logger in
    ()

  let export_dydt logger compil network split =
    let is_zero = fresh_is_zero network in
    let label = "listOfReactions" in
    let () = Ode_loggers.open_procedure logger "dydt" "ode_aux" ["t";"y"] in
    let () = Sbml_backend.open_box logger label in
    let () = Ode_loggers.print_newline logger in
    let () = Sbml_backend.line_sbml logger in
    let () =
      Ode_loggers.declare_global logger Ode_loggers_sig.N_ode_var
    in
    let () =
      Ode_loggers.declare_global logger (Ode_loggers_sig.Expr 1)
    in
    let () = declare_rates_global logger network in
    let () =
      List.iter
        (affect_var is_zero logger logger ~init_mode:false compil network) split.var_decl in
    let () = Ode_loggers.print_newline logger in
    let () =
      List.iter
        (fun (rule,rate) ->
           Ode_loggers.associate
             (I.string_of_var_id ~compil)
             logger logger
             (var_of_rule rule) rate (handler_expr network))
        split.var_rate
    in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.initialize logger (Ode_loggers_sig.Deriv 1) in
    let do_it f l reactants enriched_rule =
      List.iter
        (fun species ->
           let nauto_in_species =
             if I.do_we_count_in_embeddings compil
             then
               snd
                 (species_of_species_id network species)
             else 1
           in
           let nauto_in_lhs = enriched_rule.divide_rate_by in
           f
             logger (Ode_loggers_sig.Deriv species)
             ~nauto_in_species ~nauto_in_lhs
             (var_of_rule enriched_rule) reactants)
        l
    in
    let () =
      List.iter
        (fun (reactants, products, token_vector, enriched_rule) ->
           let add_factor l  =
             if I.do_we_count_in_embeddings compil
             then
               List.rev_map
                 (fun x ->
                    let nauto = snd
                        (species_of_species_id network x)
                    in
                    (x,
                     nauto))
                 (List.rev l)
             else
               List.rev_map
                 (fun x -> (x,1))
                 (List.rev l)
           in
           let reactants' = add_factor reactants in
           let products' = add_factor products in
           let () =
             if I.do_we_prompt_reactions compil
             then
               let rule_string =
                 Format.asprintf "%a" (I.print_rule_name ~compil) enriched_rule.rule
               in
               let tokens_prod = I.token_vector enriched_rule.rule in
               let dump_token_list fmt list =
                 let _ =
                   List.fold_left
                     (fun bool ((alg,_),k) ->
                      let prefix = if bool then " + " else " | " in
                      let () =
                        Format.fprintf fmt "%s%a:%a"
                          prefix
                          (
                             Alg_expr.print
                               (fun fmt mixture ->
                                  let () = Format.fprintf fmt "|" in
                                  let
                                    _  =
                                    List.fold_left
                                    (
                                      Array.fold_left
                                         (fun bool connected_component ->
                                            let prefix = if bool then " , " else "" in
                                            let () =
                                              Format.fprintf
                                              fmt
                                              "%s%a"
                                              prefix
                                              (I.print_connected_component ~compil)
                                              connected_component
                                            in true)
                                         )
                                         false mixture
                                  in
                                  let () = Format.fprintf fmt "|" in
                                  ())
                               (I.print_token ~compil)
                               (fun fmt var_id -> Format.fprintf fmt "%s" (I.string_of_var_id ~compil (succ var_id)))
                          )
                          alg
                          (I.print_token ~compil)
                          k
                      in true
                   ) false (List.rev list)
                 in ()
               in
               let () = Ode_loggers.print_newline logger in
               let () =
                 Ode_loggers.print_comment ~breakline logger ("rule    : "^rule_string)
               in
               let dump fmt list  =
                 let _ =
                   List.fold_left
                     (fun bool k ->
                        let prefix = if bool then " + " else "" in
                        let species_string =
                          Format.asprintf "%a"
                            (fun log id -> I.print_chemical_species ~compil log (fst (Mods.DynArray.get network.species_tab id)))
                            k
                        in
                        let () =
                          Format.fprintf fmt "%s%s"
                            prefix
                            species_string
                        in
                        true)
                     false
                     (List.rev list)
                 in ()
               in
               match
                 Loggers.get_encoding_format logger
               with
               | Loggers.Matlab | Loggers.Octave  | Loggers.SBML ->
                 let s = Format.asprintf
                     "reaction: %a -> %a%a "
                     dump reactants
                     dump products
                     dump_token_list tokens_prod
                 in
                 Ode_loggers.print_comment ~breakline logger s
               | Loggers.Matrix | Loggers.Maple | Loggers.TXT
               | Loggers.TXT_Tabular | Loggers.XLS
               | Loggers.DOT | Loggers.HTML | Loggers.HTML_Graph
               | Loggers.HTML_Tabular | Loggers.Json -> ()

           in
           let () =
             Sbml_backend.dump_sbml_reaction
               (I.string_of_var_id ~compil)
               get_rule
               I.print_rule_name
               (Some compil)
               logger
               (handler_expr network)
               reactants'
               products'
               token_vector
               enriched_rule
               (var_of_rule enriched_rule)
               enriched_rule.divide_rate_by
           in
           let reactants'  =
             List.rev_map
               (fun x ->
                  let nauto = snd
                      (species_of_species_id network x)
                  in
                  (Ode_loggers_sig.Concentration x,
                   to_nocc_correct compil nauto))
               (List.rev reactants)
           in
           let nauto_in_lhs = enriched_rule.divide_rate_by in
           let () = Ode_loggers.print_newline logger in
           let () = do_it Ode_loggers.consume reactants reactants' enriched_rule in
           let () = do_it Ode_loggers.produce products reactants' enriched_rule in
           let () =
             List.iter
               (fun (expr,(token,_loc)) ->
                  Ode_loggers.update_token
                    (I.string_of_var_id ~compil)
                    logger
                    (Ode_loggers_sig.Deriv token) ~nauto_in_lhs
                    (var_of_rule enriched_rule)
                    expr reactants' (handler_expr network))
               token_vector
           in ()
        )
        network.reactions
    in
    (* Derivative of time is equal to 1 *)
    let () =
      if may_be_not_time_homogeneous network
      then
        let () =
          Ode_loggers.associate
            (I.string_of_var_id ~compil) logger logger
            (Ode_loggers_sig.Deriv
               (get_last_ode_var_id network)) (alg_of_int 1) (handler_expr network)
        in
        let () = Ode_loggers.print_newline logger in
        let () = Sbml_backend.time_advance logger in
        ()
      else
        ()
    in
    let () = Ode_loggers.close_procedure logger in
    let () = Sbml_backend.close_box logger label in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_newline logger in
    ()

  let export_init logger compil network =
    let label = "listOfSpecies" in
    let () = Sbml_backend.open_box logger label in
    let () = Sbml_backend.line_sbml logger in
    let () = Ode_loggers.open_procedure logger "Init" "ode_init" [] in
    let () = Ode_loggers.print_newline logger in
    let () =
      Ode_loggers.declare_global logger Ode_loggers_sig.N_ode_var
    in
    let () =
      Ode_loggers.declare_global logger (Ode_loggers_sig.Init (get_last_ode_var_id network))
    in
    let () = Ode_loggers.initialize logger (Ode_loggers_sig.Initbis (get_last_ode_var_id network)) in
    let () = Ode_loggers.print_newline logger in
    let rec aux k =
      if
        k >= get_fresh_ode_var_id network
      then
        ()
      else
        let id, comment, units  =
          if
            may_be_not_time_homogeneous network &&
            k = get_fresh_ode_var_id network - 1
          then "time","t", Some "substance"
          else
            let variable = Mods.DynArray.get network.ode_vars_tab k in
            begin
              match variable with
              | Dummy -> "dummy", "", None
              | Token id ->
                "t"^(string_of_int k),
                Format.asprintf "%a"
                  (fun _log _id -> ()) id ,
                (Some "substance")
              | Noccurrences _ | Nembed _ ->
                "s"^(string_of_int k),
                Format.asprintf "%a"
                  (fun log k -> I.print_chemical_species ~compil log
                      (fst (Mods.DynArray.get network.species_tab k))) k,
                Some "substance"
            end
        in
        let () = Ode_loggers.declare_init ~comment logger k in
        let () =
          Sbml_backend.dump_initial_species
            ?units
            logger
            (handler_expr network)
            k
            comment
            id
        in
        aux (next_id k)
    in
    let () = aux fst_id in
    let () = Ode_loggers.close_procedure logger in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_newline logger in
    let () = Sbml_backend.close_box logger label in
    ()

  let export_obs logger compil network split =
    let is_zero = fresh_is_zero network in
    let () = Ode_loggers.open_procedure logger "obs" "ode_obs" ["y"] in
    (* add t *)
    let () = Ode_loggers.print_newline logger in
    let () =
      Ode_loggers.declare_global logger Ode_loggers_sig.N_obs
    in
    let () =
      Ode_loggers.declare_global logger (Ode_loggers_sig.Expr 1)
    in
    let () =
      Ode_loggers.initialize logger (Ode_loggers_sig.Obs (network.n_obs))
    in
    let () = Ode_loggers.print_newline logger in
    let () =
      if
        obs_may_be_not_time_homogeneous network
      then
        Ode_loggers.associate_t logger
          (get_last_ode_var_id network)
      else
        ()
    in
    let () =
      List.iter
        (affect_var is_zero logger logger ~init_mode:false compil network) split.var_decl
    in
    let () = Ode_loggers.print_newline logger in
    let () =
      List.iter
        (fun (id,expr) ->
           Ode_loggers.associate
             (I.string_of_var_id ~compil) logger logger (Ode_loggers_sig.Obs id) expr (handler_expr network))
        network.obs
    in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.close_procedure logger in
    let () = Ode_loggers.print_newline logger in
    let () = Ode_loggers.print_newline logger in
    ()

  let export_network
      ~command_line ~command_line_quotes ~data_file ~init_t ~max_t ~plot_period
      logger logger_buffer compil network =
    let network =
      if
        may_be_not_time_homogeneous network
      then
        (* add a spurious variable for time *)
        inc_fresh_ode_var_id network
      else
        network
    in
    let sorted_rules_and_decl =
      split_rules_and_decl compil network
    in
    let () = Format.printf "+ exporting the network... @." in
    let () = Format.printf "\t -main function @." in
    let () =
      export_main
        ~command_line ~command_line_quotes ~data_file ~init_t ~max_t ~plot_period
        logger logger_buffer compil network sorted_rules_and_decl
    in
    let () = Format.printf "\t -initial state @." in
    let () = export_init logger compil network in
    let () =
      match
        Loggers.formatter_of_logger logger
      with
      | None -> ()
      | Some fmt -> Loggers.flush_buffer logger_buffer fmt
    in
    let () = Format.printf "\t -ode system @." in
    let () = export_dydt logger compil network sorted_rules_and_decl in
    let () = Format.printf "\t -observables @." in
    let () = export_obs logger compil network sorted_rules_and_decl in
    let () = Ode_loggers.launch_main logger in
    ()

  let get_reactions network =
    let list = get_reactions network in
    List.rev_map
      (fun (a,b,c,d)-> (a,b,c,d.rule))
      (List.rev list)

  (**********************************************************)
  (*morphism_per_rules*)

  let print_hash log hash =
    Loggers.fprintf log "Hash-:%a\n"
      LKappa_auto.RuleCache.print hash

    let print_line_stars log =
      Loggers.fprintf log "***************\n"

  let print_canonic_form_from_syntactic_rule log hash_list =
    List.iter (fun hash ->
        let () = print_line_stars log in
        let () = print_hash log hash in
        let () = print_line_stars log in ()
      ) hash_list

  let print_map log map =
    let () = print_line_stars log in
    let () = Loggers.print_newline log in
    let () =
      LKappa_auto.CannonicMap.fold (fun cannonic (nocc, nauto) () ->
          let () = Loggers.fprintf log "nocc:%i - " nocc in
          let () = Loggers.fprintf log "nauto:%i - " nauto in
          let () = Loggers.fprintf log "cannonic:%a"
              LKappa_auto.CannonicCache.print cannonic
          in
          let () = Loggers.print_newline log in ()
        ) map ()
    in
    let () = print_line_stars log in
    ()

  let print_divide_rule_rate_by log i =
    Loggers.fprintf log " Divide_rule_rate_by:%i\n" i

  let print_kinetic_rate_list ?env log fmt l =
    List.iter (fun rate_opt ->
        match rate_opt with
        | None -> ()
        | Some (alg, loc) ->
          (*let () = Locality.print fmt loc in*)
          let () = Loggers.fprintf log " Rate: " in
          let () = Kappa_printer.alg_expr ?env fmt alg in
          let () = Loggers.print_newline log in
          ()
      ) l

  (******************************************************************)
  (*We say that the rules have a symmetric action over the site x and
    the site y, whenever the the product between the number of
    automorphisms of the rule r and its kinetic rate k(r), divided by
    the number of automorphisms in the left hand side of the rule r,
    is the same for any pair of symmetric rules.*)

  let compute_gamma log connected_components nbr_auto_in_lhs =
    List.fold_left (fun current_list l ->
        List.fold_left (fun current_list' arr ->
            Array.fold_left (fun current_array i ->
                let k = Pattern.int_of_id i in
                let g = k / nbr_auto_in_lhs in
                g :: current_array
              ) current_list' arr
          ) current_list l
      ) [] (List.flatten connected_components)

  let compute_isomorphism_in_hashes nbr_auto_in_rule_list =
    let rec aux acc l =
    match l with
    | [] | [_] -> acc
    | x :: tl ->
      if List.mem x tl
      then aux (x::x::acc) tl
      else aux acc tl
    in
    aux [] nbr_auto_in_rule_list

  let print_isomorphism log list =
    let () =
      List.iter (fun hash ->
          let () =
            Loggers.fprintf log
              "List of hashes that are equals "
          in
          print_hash log hash
        ) list
    in
    ()

  let cannonic_form_from_syntactic_rules log compil =
    let empty_cache = I.empty_lkappa_cache () in
    let cache, (*gamma_list, *)nbr_auto_in_rule_list =
      List.fold_left (fun (cache, (*_gammar,*) current_list) rule ->
          (* convention of r:
            the number of automorphisms in the lhs of the rule r*)
          let cache, nbr_auto_in_lhs =
            I.divide_rule_rate_by cache compil rule
          in
          (* identifiers of rule up to isomorphism*)
          let rate_opt_list, cache, nbr_auto_in_rule =
            I.cannonic_form_from_syntactic_rule cache compil rule
          in
          (*compute gamma(r)= k(r)/convention(r)*)
          (*Pattern.id array list list*)
          let connected_components =
            List.fold_left (fun current_list rate_opt ->
                match rate_opt with
                | None -> current_list
                | Some annot ->
                  (*extract connected components:
                    pattern_id array list *)
                  let connected_components =
                      Alg_expr.extract_connected_components annot
                  in
                  connected_components :: current_list
              ) [] rate_opt_list
          in
          let gamma_list =
            compute_gamma log connected_components nbr_auto_in_lhs
          in
          (*****************************************************)
          (*PRINT*)
          let () =
            match Loggers.formatter_of_logger log with
            | None -> ()
            | Some fmt ->
              let () = I.print_rule_name ~compil fmt rule in
              let () = print_divide_rule_rate_by log nbr_auto_in_lhs in
              let () = print_hash log nbr_auto_in_rule in
              let () = print_kinetic_rate_list log fmt rate_opt_list in
              ()
          in
          cache, (*gamma_list,*)
          nbr_auto_in_rule :: current_list
        ) (empty_cache,[]) (I.get_rules compil)
    in
    (*compute the isomorphism of two rules:
      if the hashes of two rules are the same then they are
      isomorphism.
      sigma(ri) = ri'
    *)
    let isomorphism_rule =
      compute_isomorphism_in_hashes nbr_auto_in_rule_list
    in
    let () = print_isomorphism log isomorphism_rule in
    let () = Ode_loggers.print_newline log in
    cache, ()

(*
[Strongly symmetric model]:
iff
1. for any element i,j in I such that i different than j,
 we have ri different than rj;
2. for any element i in I and any pair of permutation sigma in
Gri , there exists an element j in I, such that:
(a) sigma.ri = rj.
(b) ki/[Li,Li] = kj/ [Lj ,Lj]
where:
i. Li is the left hand side of the rule ri,
ii. Lj is the left hand side of the rule rj,
iii. and for any site graph [E, E] denotes the number of
automorphisms in the site graph E.
*)




end
