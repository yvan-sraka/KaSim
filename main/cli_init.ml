let get_compilation ?max_event cli_args =
  let plot_period =
    match cli_args.Run_cli_args.maxTimeValue,
          cli_args.Run_cli_args.nb_points with
    | Some max_t, Some points when cli_args.Run_cli_args.plotPeriod = 1. ->
      (max_t -. cli_args.Run_cli_args.minTimeValue) /. float_of_int points
    | _, _ ->
      let () =
        if cli_args.Run_cli_args.nb_points <> None then
          ExceptionDefn.warning
            (fun f ->
               Format.pp_print_string f "-p has been ignored") in
      cli_args.Run_cli_args.plotPeriod in
  let counter =
    Counter.create
      ~init_t:cli_args.Run_cli_args.minTimeValue
      ~init_e:0
      ?max_time:cli_args.Run_cli_args.maxTimeValue
      ?max_event ~plot_period in
  let (env, contact_map, updated_vars, story_compression,
       unary_distances, formatCflows, init_l),
      counter,alg_overwrite =
    match cli_args.Run_cli_args.marshalizedInFile with
    | "" ->
      let result =
        List.fold_left (KappaLexer.compile Format.std_formatter)
          Ast.empty_compil cli_args.Run_cli_args.inputKappaFileNames in
      let () = Format.printf "+ Sanity checks@." in
      let (sigs_nd,tk_nd,updated_vars,result') =
        LKappa.compil_of_ast cli_args.Run_cli_args.alg_var_overwrite result in
      let () = Format.printf "+ KaSa tools initialization@." in
      let contact_map,_kasa_state =
        Eval.init_kasa Remanent_parameters_sig.KaSim sigs_nd result in
      let () = Format.printf "+ Compiling...@." in
      let (env, story_compression, unary_distances, formatCflow, init_l)=
        Eval.compile
          ~pause:(fun f -> f ())
          ~return:(fun x -> x)
          ?rescale_init:cli_args.Run_cli_args.rescale
          ~outputs:(Outputs.go (Signature.create [||]))
          sigs_nd tk_nd contact_map result' in
      (env, contact_map, updated_vars, story_compression,
       unary_distances, formatCflow, init_l),counter,[]
    | marshalized_file ->
      try
        let d = open_in_bin marshalized_file in
        let () =
          if cli_args.Run_cli_args.inputKappaFileNames <> [] then
            ExceptionDefn.warning
              (fun f ->
                 Format.pp_print_string
                   f "Simulation package loaded, all kappa files are ignored") in
        let () = Format.printf "+ Loading simulation package %s...@."
            marshalized_file in
        let env,contact_map,updated_vars,story_compression,
            unary_distances,formatCflow,init_l =
          (Marshal.from_channel d :
             Environment.t*Primitives.contact_map*
             int list* (bool*bool*bool) option*bool option*Ast.formatCflow*
             (Alg_expr.t * Primitives.elementary_rule * Location.t) list) in
        let () = Pervasives.close_in d  in
        let alg_overwrite =
          List.map
            (fun (s,v) ->
               Environment.num_of_alg (Location.dummy_annot s) env,
               Alg_expr.CONST v)
            cli_args.Run_cli_args.alg_var_overwrite in
        let updated_vars' =
          List.fold_left
            (fun acc (i,_) -> i::acc) updated_vars alg_overwrite in
        (env,contact_map,updated_vars',story_compression,
         unary_distances,formatCflow,init_l),
        counter,alg_overwrite
      with
      | ExceptionDefn.Malformed_Decl _ as e -> raise e
      | _exn ->
        Debug.tag
          Format.std_formatter
          "!Simulation package seems to have been created with a different version of KaSim, aborting...@.";
        exit 1 in
  (env, contact_map, updated_vars, story_compression,
   unary_distances, formatCflows, init_l),counter,alg_overwrite
