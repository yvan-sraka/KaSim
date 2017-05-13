(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type directive_unit = Time | Event

let get_compilation
    ?(unit=Time) ?(max_sharing=false) ?bwd_bisim ?(quiet=false) ?(compileModeOn=false)
    cli_args =
  let (conf, progressConf, env0, contact_map, updated_vars, story_compression,
       formatCflows, cflowFile, init_l),
      alg_overwrite,overwrite_t0 =
    match cli_args.Run_cli_args.marshalizedInFile with
    | "" ->
      let fmt =
        if quiet
        then (Format.make_formatter
                (fun _ _ _ -> ())
                (fun _ -> ()))
        else Format.std_formatter
      in
      let result =
        List.fold_left (KappaLexer.compile fmt)
          Ast.empty_compil cli_args.Run_cli_args.inputKappaFileNames in
      let () = if quiet then () else Format.printf "+ simulation parameters@." in
      let conf, progressConf,
          (n,w,s as story_compression), formatCflow, cflowFile =
        Configuration.parse result.Ast.configurations in
      let () = if quiet then () else Format.printf "+ Sanity checks@." in
      let new_syntax =
        (cli_args.Run_cli_args.newSyntax || conf.Configuration.newSyntax) in
      let (sigs_nd,contact_map,tk_nd,alg_finder,updated_vars,result') =
        LKappa.compil_of_ast
          ~new_syntax
          cli_args.Run_cli_args.alg_var_overwrite result in
      let overwrite_init,overwrite_t0 = match cli_args.Run_cli_args.initialMix with
        | None -> None,None
        | Some file ->
          let compil =
            KappaLexer.compile Format.std_formatter Ast.empty_compil file in
          let conf, _, _, _, _ =
            Configuration.parse compil.Ast.configurations in
          Some
            (LKappa.init_of_ast
               ~new_syntax sigs_nd contact_map tk_nd.NamedDecls.finder
               alg_finder compil.Ast.init),
          conf.Configuration.initial in
      let () = if quiet then () else Format.printf "+ Compiling...@." in
      let outputs,outputs' =
        if quiet then
          let sigs = Signature.create [||] in
          (fun _ -> ()),
          Some (Outputs.go sigs)
        else
          Outputs.go (Signature.create [||]), None
      in
      let (env, has_tracking,init_l) =
        Eval.compile
          ~outputs ?outputs'
          ~pause:(fun f -> f ()) ~return:(fun x -> x) ~max_sharing
          ?rescale_init:cli_args.Run_cli_args.rescale
          ?overwrite_init ?bwd_bisim ~compileModeOn
          sigs_nd tk_nd contact_map result' in
      let story_compression =
        if has_tracking && (n||w||s) then Some story_compression else None in
      (conf, progressConf, env, contact_map, updated_vars, story_compression,
       formatCflow, cflowFile,init_l),[],overwrite_t0
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
        let conf,progress,env,contact,updated,compr,cflow,cflowfile,_ as pack =
          (Marshal.from_channel d :
             Configuration.t*Counter.progressBar*Model.t*Contact_map.t*int list*
             (bool*bool*bool) option*string*string option*
             (Alg_expr.t * Primitives.elementary_rule * Locality.t) list) in
        let () = Pervasives.close_in d  in
        let alg_overwrite =
          List.map
            (fun (s,v) ->
               Model.num_of_alg (Locality.dummy_annot s) env,
               Alg_expr.CONST v)
            cli_args.Run_cli_args.alg_var_overwrite in
        match cli_args.Run_cli_args.initialMix with
        | None -> pack,alg_overwrite,None
        | Some file ->
          let compil =
            KappaLexer.compile Format.std_formatter Ast.empty_compil file in
          let raw_inits =
            LKappa.init_of_ast
              ~new_syntax:cli_args.Run_cli_args.newSyntax
              (Model.signatures env) contact (Model.tokens_finder env)
              (Model.algs_finder env) compil.Ast.init in
          let conf', _, _, _, _ =
            Configuration.parse compil.Ast.configurations in
          let inits = Eval.compile_inits
              ~compileModeOn:false contact env raw_inits in
          (conf,progress,env,contact,updated,compr,cflow,cflowfile,inits),
          alg_overwrite,conf'.Configuration.initial
      with
      | ExceptionDefn.Malformed_Decl _ as e -> raise e
      | _exn ->
        Debug.tag
          Format.std_formatter
          "!Simulation package seems to have been created with a different version of KaSim, aborting...@.";
        exit 1 in
  let init_t_from_files =
    Option_util.unsome
      (Option_util.unsome 0. conf.Configuration.initial)
      overwrite_t0 in
    let init_t,max_time,init_e,max_event,plot_period =
    match unit with
    | Time ->
      Option_util.unsome init_t_from_files cli_args.Run_cli_args.minValue,
      cli_args.Run_cli_args.maxValue,
      None,None,
      (match cli_args.Run_cli_args.plotPeriod with
       | Some a -> Counter.DT a
       | None ->
         Option_util.unsome (Counter.DT 1.) conf.Configuration.plotPeriod)
    | Event ->
      init_t_from_files,None,
      Some (int_of_float (Option_util.unsome 0. cli_args.Run_cli_args.minValue)),
      Option_util.map int_of_float cli_args.Run_cli_args.maxValue,
      match cli_args.Run_cli_args.plotPeriod with
      | Some a -> Counter.DE (int_of_float (ceil a))
      | None ->
        Option_util.unsome (Counter.DE 1) conf.Configuration.plotPeriod in
  let counter =
    Counter.create ~init_t ?init_e ?max_time ?max_event ~plot_period in
  let env =
    if cli_args.Run_cli_args.batchmode then
      Model.propagate_constant
        ?max_time:(Counter.max_time counter)
        ?max_events:(Counter.max_events counter) updated_vars alg_overwrite env0
    else Model.overwrite_vars alg_overwrite env0 in

  (conf, progressConf, env, contact_map, updated_vars, story_compression,
   formatCflows, cflowFile, init_l),counter
