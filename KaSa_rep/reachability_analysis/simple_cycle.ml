(* for each right member find all the cycles *)

let find_cycle parameters error (compil:Cckappa_sig.compil)=


  (*tranforme compil into right member*)

  (** FOR EACH MEMBER : WORK IN PROGRESS **)

  (*rules : enriched_rule Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.t;*)
  let rule = compil.Cckappa_sig.rules in (*on a le tableau et faut faire un get jusuq recuperer la partie droite*)
  (*blem : comment recuperer l'id ?? (rule_id:Ckappa_sig.c_rule_id) fold? *)
  (*in
    let r_id = Ckappa_sig.read_c_rule_id (Yojson.Safe.init_lexer ()) (Lexing.from_string s) in*)
  let key_list = Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.key_list in
  let list_cycle_graph key_list =
    List.map(fun r_id->
        let error, enruleo = Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get parameters error r_id rule
        (**)
        in
        match enruleo with
        | None -> []
        | Some enrule ->
          begin
            (*in*)
            let mixture   =
              enrule.Cckappa_sig.e_rule_c_rule.Cckappa_sig.rule_rhs in

            (* transforme the mixture into a graph  and give all the cycle*)
            let error,cycleingraph = Graphs.give_cycle parameters error mixture in

             cycleingraph
          end

      )  key_list
  in  list_cycle_graph  key_list




    (*let get_rule parameters error static r_id =
      let compil = get_compil static in
      let error, rule  =
        Ckappa_sig.Rule_nearly_Inf_Int_storage_Imperatif.get
          parameters
          error
          r_id
          compil.Cckappa_sig.rules
      in
      error, rule*)
