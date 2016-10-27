(**
  * covering_classes.ml
  * openkappa
  * Jérôme Feret & Ly Kim Quyen, projet Abstraction, INRIA Paris-Rocquencourt
  *
  * Creation: 2015, the 23th of Feburary
  * Last modification: Time-stamp: <Oct 25 2016>
  *
  * Compute the relations between the left hand site of a rule and its sites.
  *
  * Copyright 2010,2011,2012,2013,2014 Institut National de Recherche en Informatique et
  * en Automatique.  All rights reserved.  This file is distributed
  * under the terms of the GNU Library General Public License *)

let trace = false

(************************************************************************************)

let compare_unit_covering_class_id _ _ = Covering_classes_type.dummy_cv_id

let collect_modified_map parameters error diff_reverse store_modified_map =
  Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold parameters error
    (fun parameters error _agent_id site_modif store_modified_map ->
       (*if there is no modified sites then do nothing*)
       if Ckappa_sig.Site_map_and_set.Map.is_empty site_modif.Cckappa_sig.agent_interface
       then error, store_modified_map
       else
         let agent_type = site_modif.Cckappa_sig.agent_name in
         let error', store_site =
           Ckappa_sig.Site_map_and_set.Map.fold
             (fun site _port (error,current_map) ->
                (*store site map*)
                let error,site_map =
                  Ckappa_sig.Site_map_and_set.Map.add
                    parameters
                    error
                    site
                    site
                    current_map
                in
                error,site_map)
             site_modif.Cckappa_sig.agent_interface
             (error, Ckappa_sig.Site_map_and_set.Map.empty)
         in
         let error =
           Exception.check_point
             Exception.warn parameters error error' __POS__ Exit
         in
         (*compute site_map*)
         let error, old_map =
           match
             Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
               parameters
               error
               agent_type
               store_modified_map
           with
           | error, None -> error, Ckappa_sig.Site_map_and_set.Map.empty
           | error, Some m -> error, m
         in
         (*store*)
         let error,final_map =
           Ckappa_sig.Site_map_and_set.Map.union
             parameters
             error
             old_map
             store_site
         in
         let error', store_modified_map =
           Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
             parameters
             error
             agent_type
             final_map
             store_modified_map
         in
         let error =
           Exception.check_point
             Exception.warn parameters error error' __POS__ Exit
         in
         error, store_modified_map
    ) diff_reverse
    store_modified_map

(*-------------------------------------------------------------------------*)
(*compute covering classes, site test and bdu*)

let collect_covering_classes parameters error views diff_reverse store_result =
  let error, store_result =
    Ckappa_sig.Agent_id_quick_nearly_Inf_Int_storage_Imperatif.fold2_common
      parameters error
      (fun parameters error _agent_id agent site_modif store_result ->
         (* if in the interface there is no site modified then do nothing *)
         if Ckappa_sig.Site_map_and_set.Map.is_empty
             site_modif.Cckappa_sig.agent_interface
         then error, store_result
         else
           match agent with
           | Cckappa_sig.Ghost
           | Cckappa_sig.Unknown_agent _ -> error, store_result
           | Cckappa_sig.Dead_agent (agent, _, _, _)
           | Cckappa_sig.Agent agent ->
             let agent_type = agent.Cckappa_sig.agent_name in
             (*get a list of sites from an interface at each rule*)
             let site_list =
               Ckappa_sig.Site_map_and_set.Map.fold
                 (fun site _ current_list ->
                    site :: current_list
                 ) agent.Cckappa_sig.agent_interface []
             in
             (*compute covering_class*)
             match site_list with
             | [] -> error, store_result
             | _ ->
               let error, old_list =
                 match
                   Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
                     parameters
                     error
                     agent_type
                     store_result
                 with
                 | error, None -> error, []
                 | error, Some l -> error, l
               in
               let new_pair_list = (List.rev site_list) :: old_list in
               let error, store_result =
                 Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
                   parameters
                   error
                   agent_type
                   new_pair_list
                   store_result
               in
               error, store_result
      ) views diff_reverse store_result
  in error, store_result

(************************************************************************************)
(*compute covering class: it is a covering class whenever there is a
  modified site in that agent. (CHECK on their left-hand side)

  For example: A(x~u,y~u) -> A(x~u,y~p) (where y is a modified site), then
  there is one covering class for agent A: CV_1: (x,y)

  - If the rule is: A(x~u), A(y~u) -> A(x~p), A(y~p), (x,y are modified
  sites), then agent A has two covering classes: CV_1: x; CV_2: y

  - If the rule is: A(x~u), A(y~u) -> A(x~u), A(y~p), (y is a modified
  site), then agent A has only one covering class: CV_1: y
*)

let scan_rule_covering_classes parameters error _handler rule classes =
  (*----------------------------------------------------------------------*)
  (*compute modified map*)
  let error, store_modified_map =
    collect_modified_map
      parameters
      error
      rule.Cckappa_sig.diff_reverse
      classes.Covering_classes_type.store_modified_map
  in
  (*----------------------------------------------------------------------*)
  (*compute covering_class*)
  let error, store_covering_classes =
    collect_covering_classes
      parameters
      error
      rule.Cckappa_sig.rule_lhs.Cckappa_sig.views
      rule.Cckappa_sig.diff_reverse
      classes.Covering_classes_type.store_covering_classes
  in
  (*----------------------------------------------------------------------*)
  (*result*)
  error,
  {
    Covering_classes_type.store_modified_map     = store_modified_map;
    Covering_classes_type.store_covering_classes = store_covering_classes;
  }

(***************************************************************************)
(*RULES*)

let scan_rule_set_covering_classes parameters error handler rules =
  let n_agents = handler.Cckappa_sig.nagents in
  let error, init_modif_map =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create_biggest_key parameters error n_agents in
  let error, init_class =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create_biggest_key parameters error n_agents in
  (*----------------------------------------------------------------------*)
  (* add each singleton as a covering class *)
  let error, init_class =
    Ckappa_sig.Agent_type_nearly_Inf_Int_storage_Imperatif.fold
      parameters
      error
      (fun parameters error agent_type b init_class ->
         Ckappa_sig.Dictionary_of_sites.fold
           (fun _ _ b (error, init_class) ->
              let error, l' =
                match
                  Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
                    parameters
                    error
                    agent_type
                    init_class
                with
                | error,None -> error, [[b]]
                | error,Some l -> error, [b]::l
              in
              Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
                parameters
                error
                agent_type
                l'
                init_class
           )
           b (error, init_class)
      )
      handler.Cckappa_sig.sites
      init_class
  in
  (*-----------------------------------------------------------------------*)
  (*init state of covering class*)
  let init_class =
    {
      Covering_classes_type.store_modified_map     = init_modif_map;
      Covering_classes_type.store_covering_classes = init_class;
    }
  in
  (*---------------------------------------------------------------------*)
  (*map each agent to a list of covering classes*)
  let error, store_covering_classes =
    Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.fold
      parameters error
      (fun parameters error _rule_id rule classes ->
         let error, result =
           scan_rule_covering_classes
             parameters
             error
             handler
             rule.Cckappa_sig.e_rule_c_rule
             classes
         in
         error, result
      ) rules init_class
  in
  error, store_covering_classes

(***************************************************************************)
(*clean covering classes*)

let length_sorted (l: Ckappa_sig.c_site_name list list):
  Ckappa_sig.c_site_name list list =
  let list_length = List.rev_map (fun list -> list, List.length list) l in
  let lists       =
    List.sort (fun a b -> compare (snd a) (snd b)) list_length in
  List.rev_map fst lists

(******************************************************************************)
(*CLEANING*)

let store_remanent parameters error covering_class _modified_map remanent =
  (* current state of remanent*)
  let pointer_backward    =
    remanent.Covering_classes_type.store_pointer_backward in
  let good_covering_class = remanent.Covering_classes_type.store_dic in
  (*-------------------------------------------------------------------------*)
  (*covering class dictionary*)
  let error, output =
    Covering_classes_type.Dictionary_of_List_sites.allocate
      parameters
      error
      compare_unit_covering_class_id
      covering_class (*value: c_site_name list*)
      ()
      Misc_sa.const_unit
      good_covering_class
  in
  let error, (cv_id, store_dic) =
    match output with
    | Some (id, _, _, dic) -> error, (id, dic)
    | None ->
      Exception.warn
        parameters error __POS__ Exit
        (Covering_classes_type.dummy_cv_id, good_covering_class)
  in
  (*-----------------------------------------------------------------------*)
  (*store pointer backward*)
  let error, pointer_backward =
    List.fold_left (fun (error, pointer_backward) old_cv_id ->
        let error, old_cv_set =
          match
            Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
              parameters
              error
              old_cv_id
              pointer_backward
          with
          | error, None -> error, Covering_classes_type.CV_map_and_set.Set.empty
          | error, Some s -> error, s
        in
        let error', new_cv_set =
          Covering_classes_type.CV_map_and_set.Set.add
            parameters
            error
            cv_id
            old_cv_set
        in
        let error =
          Exception.check_point
            Exception.warn parameters error error' __POS__ Exit
        in
        let error, pointer_backward =
          Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.set
            parameters
            error
            old_cv_id (*int*)
            new_cv_set (*set of int*)
            pointer_backward
        in
        error, pointer_backward
      )
      (error, pointer_backward)
      covering_class (*c_site_name list*)
  in
  (*--------------------------------------------------------------------*)
  (*result*)
  error,
  {
    Covering_classes_type.store_pointer_backward    = pointer_backward;
    Covering_classes_type.store_dic                 = store_dic;
  }

(*--------------------------------------------------------------------------*)
(*CLEAN: In a covering class, it will store the old result of the previous
  covering class of an agent.

  For example:
  - rule 0: agent A has a covering class: (0)
  - rule 1: agent A has a covering class: (0,1)
  => Then do the intersection of two covering classes of agent A:
  (0) inter (0,1) -> 0
*)

let clean_classes parameters error covering_classes modified_map =
  let error, init_pointer =
    Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.create parameters error 0
  in
  let init_store_dic = Covering_classes_type.Dictionary_of_List_sites.init () in
  (*------------------------------------------------------------------------*)
  (*init state of dictionary*)
  let init_remanent =
    {
      Covering_classes_type.store_pointer_backward = init_pointer;
      Covering_classes_type.store_dic              = init_store_dic;
    }
  in
  (*------------------------------------------------------------------------*)
  (*cleaning*)
  let current_covering_classes = length_sorted covering_classes in
  List.fold_left (fun (error, remanent) covering_class ->
      match covering_class with
      | [] -> error, remanent
      | t :: tl ->
        let pointer_backward = remanent.Covering_classes_type.store_pointer_backward in
        (* return the set of list(id) containing t.
           For example: current_covering_classes: [[0;1];[0]]
           t = 0 => (id:1;id:2) of type set;
           remanent_type: [(id:1,[0;1]);(id:2,[0])];
           (id:pointer_backward, dic: int list)
        *)
        let error, potential_supersets =
          match
            Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
              parameters
              error
              t
              pointer_backward
          with
          | error, None -> error, Covering_classes_type.CV_map_and_set.Set.empty
          | error, Some set -> error, set
        in
        let rec aux to_visit potential_supersets =
          match to_visit with
          | [] -> error, remanent
          | t' :: tl' ->
            (* get the set of list(id) containing t' *)
            let error, potential_supersets' =
              match
                Ckappa_sig.Site_type_nearly_Inf_Int_storage_Imperatif.unsafe_get
                  parameters
                  error
                  t'
                  pointer_backward
              with
              | error, None -> error, Covering_classes_type.CV_map_and_set.Set.empty
              | error, Some set -> error, set
            in
            (*------------------------------------------------------------*)
            (* intersection of two sets *)
            let error',potential_superset =
              Covering_classes_type.CV_map_and_set.Set.inter
                parameters
                error
                potential_supersets
                potential_supersets'
            in
            let error =
              Exception.check_point
                Exception.warn parameters error error'
                __POS__ Exit
            in
            if
              Covering_classes_type.CV_map_and_set.Set.is_empty
                potential_superset
            then
              let error, result_covering_dic =
                store_remanent
                  parameters
                  error
                  covering_class
                  modified_map
                  remanent
              in
              error, result_covering_dic
            else
              aux tl' potential_superset
        in
        (*-------------------------------------------------------------------*)
        (*check the beginning state of a superset*)
        if Covering_classes_type.CV_map_and_set.Set.is_empty potential_supersets
        then
          (*if it is empty then store it to remanent*)
          let error, result_covering_dic =
            store_remanent
              parameters
              error
              covering_class
              modified_map
              remanent
          in
          error, result_covering_dic
        else
          aux tl potential_supersets
    )
    (error, init_remanent)
    current_covering_classes

(*-------------------------------------------------------------------------*)
(*compute covering classes in the set of rules*)

let scan_rule_set_remanent parameters error handler rules =
  (*create a new initial state to store after cleaning the covering classes*)
  let error, init_result =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create parameters error 0
  in
  let error, store_covering_classes =
    scan_rule_set_covering_classes parameters error handler rules
  in
  let result_covering_classes =
    store_covering_classes.Covering_classes_type.store_covering_classes
  in
  let error, remanent_dictionary =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.fold parameters error
      (fun parameters error agent_type covering_class init_remanent ->
         (*----------------------------------------------------------------*)
         (*get modified site*)
         let error, modified_map =
           match
             Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.unsafe_get
               parameters
               error
               agent_type
               store_covering_classes.Covering_classes_type.store_modified_map
           with
           | error, None -> error, Ckappa_sig.Site_map_and_set.Map.empty
           | error, Some m -> error, m
         in
         (*-----------------------------------------------------------------*)
         (*clean the covering classes, removed duplicate of covering classes*)
         let error, store_remanent_dic =
           clean_classes
             parameters
             error
             covering_class
             modified_map
         in
         (*---------------------------------------------------------------*)
         (*compute the number of covering classes*)
         let error, get_number_cv =
           (Covering_classes_type.Dictionary_of_List_sites.last_entry
              parameters error
              store_remanent_dic.Covering_classes_type.store_dic)
         in
         let number_cv = (Covering_classes_type.int_of_cv_id get_number_cv) + 1 in
         (*----------------------------------------------------------------*)
         (*print covering classes*)
         let _ =
           if Remanent_parameters.get_dump_site_dependencies parameters
           then
             let parameters =
               Remanent_parameters.update_prefix parameters ""
             in
             let error, agent_string =
               Handler.string_of_agent parameters error handler agent_type
             in
             let _ =
               Covering_classes_type.Dictionary_of_List_sites.iter parameters error
                 (fun parameters error elt_id(*key*) site_type_list(*value*) _
                   _ ->
                    let _ =
                      Printf.fprintf stdout
                        "Potential dependencies between sites:Number of covering classes:%i\n"
                        number_cv
                    in
                    let _ =
                      (*print covering_class_id*)
                      Printf.fprintf stdout
                        "Potential dependencies between sites:\nagent_type:%s:%s:covering_class_id:%i\n"
                        (Ckappa_sig.string_of_agent_name agent_type)
                        agent_string
                        (Covering_classes_type.int_of_cv_id elt_id)
                    in
                    let error  =
                      List.fold_left (fun error site_type ->
                          let error, site_string =
                            Handler.string_of_site parameters error handler
                              agent_type site_type
                          in
                          let () =
                            Printf.fprintf stdout "site_type:%i:%s\n"
                              (Ckappa_sig.int_of_site_name site_type)
                              site_string
                          in
                          error
                        ) error site_type_list
                    in
                    error
                 ) store_remanent_dic.Covering_classes_type.store_dic
             in
             ()
         in
         (*---------------------------------------------------------------*)
         (*store the covering classes after cleaning theirs duplicate classes*)
         let error, store_remanent =
           Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.set
             parameters
             error
             agent_type
             store_remanent_dic
             init_remanent
         in
         (*----------------------------------------------------------------*)
         (*result*)
         error, store_remanent
      )
      result_covering_classes
      init_result
  in
  error, remanent_dictionary

(**************************************************************************)
(*MAIN*)

let covering_classes parameters error handler cc_compil =
  (*let error, init =
    Ckappa_sig.Agent_type_quick_nearly_Inf_Int_storage_Imperatif.create parameter error 0
    in*)
  let parameters = Remanent_parameters.update_prefix parameters "agent_type:" in
  let error, result = scan_rule_set_remanent parameters error handler
      cc_compil.Cckappa_sig.rules
  in
  error, result
