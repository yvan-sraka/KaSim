(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Lwt.Infix
(* addd seed to parameter *)
let patch_parameter (simulation_parameter : Api_types_j.simulation_parameter) :
  (Api_types_j.simulation_parameter*int) =
  match simulation_parameter.Api_types_j.simulation_seed with
  | None ->
    let () = Random.self_init () in
    let seed = Random.bits () in
    ({ simulation_parameter with Api_types_j.simulation_seed = Some seed } ,
     seed)
  | Some seed -> (simulation_parameter,seed)

let bind_simulation simulation handler =
  match simulation with
  | Some (_,simulation) -> handler simulation
  | None ->
    let m  = "No simulation available" in
    Lwt.return (Api_common.result_error_msg ~result_code:`Not_found m)

let detail_projection
    ~simulation
    ~(system_process:Kappa_facade.system_process)
    ~(projection:(Api_data.simulation_detail_output -> 'a Api.result))
  : 'a Api.result Lwt.t =
  bind_simulation
    simulation
    (fun t ->
       (Kappa_facade.outputs
          ~system_process:system_process
          ~t:t) >>=
       (Result_util.fold
          ~ok:(fun (simulation_detail : Api_data.simulation_detail_output) ->
              Lwt.return (projection simulation_detail):
                (Api_data.simulation_detail_output -> 'a Api.result Lwt.t))
          ~error:(fun errors ->
              Lwt.return (Api_common.result_messages errors))
       ))

class virtual manager_file_line
    (system_process : Kappa_facade.system_process) = object(self)

  val mutable virtual simulation :
    (Api_types_t.simulation_parameter * Kappa_facade.t) option

  method private info_file_line
      (detail : Api_data.simulation_detail_output) :
    Api_types_j.file_line_catalog Api.result =
    let file_lines : string list Mods.StringMap.t =
      detail.Api_types_t.simulation_output_file_lines in
    let file_line_ids : string list =
      List.map fst (Mods.StringMap.bindings file_lines) in
    let file_line_catalog = { Api_types_j.file_line_ids } in
    Result_util.ok file_line_catalog

  method private get_file_line
      ~file_line_id
      (status : Api_data.simulation_detail_output) :
    (string list) Api.result =
    let file_line_list = status.Api_types_j.simulation_output_file_lines in
    match Mods.StringMap.find_option file_line_id file_line_list with
    | None ->
      let m : string = Format.sprintf "id %s not found"  file_line_id in
      Api_common.result_error_msg ~result_code:`Not_found m
    | Some lines -> Result_util.ok (List.rev lines)

  method simulation_catalog_file_line :
    Api_types_j.file_line_catalog Api.result Lwt.t =
    detail_projection ~simulation ~system_process ~projection:self#info_file_line

  method simulation_detail_file_line
      (file_line_id : string) : string list Api.result Lwt.t =
    detail_projection
      ~simulation ~system_process ~projection:(self#get_file_line ~file_line_id)
end

class virtual manager_flux_map
    (system_process : Kappa_facade.system_process) = object(self)

  val mutable virtual simulation :
    (Api_types_t.simulation_parameter * Kappa_facade.t) option

  method private info_flux_map
      (detail : Api_data.simulation_detail_output) :
    Api_types_j.din_catalog Api.result =
    let flux_maps : Api_types_j.din list =
      detail.Api_types_j.simulation_output_dins in
    let flux_map_catalog =
      { Api_types_j.din_ids =
          List.map (fun f -> f.Data.din_data.Data.din_name)
            flux_maps } in
    Result_util.ok flux_map_catalog

  method private get_flux_map
      (flux_map_id : Api_types_j.din_id)
      (detail : Api_data.simulation_detail_output) :
    Api_types_j.din Api.result =
    let flux_maps_list : Api_types_j.din list =
      detail.Api_types_j.simulation_output_dins in
    let flux_maps_eq : Api_types_j.din -> bool =
      fun flux_map ->
        flux_map_id = flux_map.Data.din_data.Data.din_name in
    try Result_util.ok (List.find flux_maps_eq flux_maps_list)
    with Not_found ->
      let m : string = Format.sprintf "id %s not found" flux_map_id in
      Api_common.result_error_msg ~result_code:`Not_found m

  method simulation_catalog_din :
    Api_types_j.din_catalog Api.result Lwt.t =
    detail_projection ~simulation ~system_process ~projection:self#info_flux_map

  method simulation_detail_din
      (flux_map_id : Api_types_j.din_id) :
    Api_types_j.din Api.result Lwt.t =
    detail_projection
      ~simulation ~system_process ~projection:(self#get_flux_map flux_map_id)
end

class virtual manager_log_message
    (system_process : Kappa_facade.system_process) = object(self)

  val mutable virtual simulation :
    (Api_types_t.simulation_parameter * Kappa_facade.t) option

  method private log_message
      (detail : Api_data.simulation_detail_output) :
    Api_types_j.log_message Api.result =
    Result_util.ok detail.Api_types_j.simulation_output_log_messages

  method simulation_detail_log_message :
    Api_types_j.log_message Api.result Lwt.t =
    detail_projection
      ~simulation ~system_process ~projection:self#log_message
end

let select_observables
    (plot_limit : Api_types_j.plot_limit)
    (plot : Api_types_j.plot) : Api_types_j.plot =
  let plot_time_series = Tools.array_rev_of_list plot.Data.plot_series in
  let plot_detail_size = Array.length plot_time_series in
  let plot_limit_offset = plot_limit.Api_types_j.plot_limit_offset in
  let plot_limit_points = plot_limit.Api_types_j.plot_limit_points in
  let start,len =
    match plot_limit_offset, plot_limit_points with
    | None, None -> 0, plot_detail_size
    | Some offset, None -> offset, max 0 (plot_detail_size - offset)
    | None, Some nb -> max 0 (plot_detail_size - nb), min nb plot_detail_size
    | Some offset, Some nb -> offset, min nb (max 0 (plot_detail_size - offset))
  in
  let new_plot_time_series =
    (List.rev (Array.to_list (Array.sub plot_time_series start len))) in
  { plot with Data.plot_series = new_plot_time_series }

class virtual manager_plot
    (system_process : Kappa_facade.system_process) = object(self)

  val mutable virtual simulation :
    (Api_types_t.simulation_parameter * Kappa_facade.t) option

  method private get_plot
      (plot_limit : Api_types_j.plot_parameter)
      (detail : Api_data.simulation_detail_output) :
    Api_types_j.plot Api.result =
    match detail.Api_types_j.simulation_output_plot with
    | Some plot ->
      Result_util.ok
        (select_observables plot_limit plot)
    | None -> let m : string = "plot not available" in
      Api_common.result_error_msg ~result_code:`Not_found m

  method simulation_detail_plot
      (plot_parameter : Api_types_j.plot_parameter) :
    Api_types_j.plot Api.result Lwt.t =
    detail_projection
      ~simulation ~system_process ~projection:(self#get_plot plot_parameter)
end

class virtual manager_snapshot
    (system_process : Kappa_facade.system_process) = object(self)

  val mutable virtual simulation :
    (Api_types_t.simulation_parameter * Kappa_facade.t) option

  method private info_snapshot
      (detail : Api_data.simulation_detail_output) :
    Api_types_j.snapshot_catalog Api.result =
    let snapshots : Api_types_j.snapshot list =
      detail.Api_types_j.simulation_output_snapshots in
    let snapshot_catalog =
      { Api_types_j.snapshot_ids =
          List.map (fun s -> s.Data.snapshot_file) snapshots } in
    Result_util.ok snapshot_catalog
  method private get_snapshot
      (snapshot_id : Api_types_j.snapshot_id)
      (detail : Api_data.simulation_detail_output)
    : Api_types_j.snapshot Api.result =
    let snapshot_list : Api_types_j.snapshot list =
      detail.Api_types_j.simulation_output_snapshots in
    let snapshot_eq : Api_types_j.snapshot -> bool =
      fun snapshot -> snapshot_id = snapshot.Data.snapshot_file in
    try Result_util.ok (List.find snapshot_eq snapshot_list)
    with Not_found ->
      let m : string = Format.sprintf "id %s not found" snapshot_id in
      Api_common.result_error_msg ~result_code:`Not_found m

  method simulation_catalog_snapshot :
    Api_types_j.snapshot_catalog Api.result Lwt.t =
    (detail_projection
       ~simulation ~system_process ~projection:self#info_snapshot
     : Api_types_j.snapshot_catalog Api.result Lwt.t)

  method simulation_detail_snapshot
      (snapshot_id : Api_types_j.snapshot_id):
    Api_types_j.snapshot Api.result Lwt.t =
    ((detail_projection
        ~simulation ~system_process ~projection:(self#get_snapshot snapshot_id))
     : Api_types_j.snapshot Api.result Lwt.t)
end

class manager_simulation
    project
    (system_process : Kappa_facade.system_process) :
  Api.manager_simulation = object(self)
  val mutable simulation = None

  method simulation_load text overwrites =
    let ast = text in
    let harakiri,_ = Lwt.task () in
    let _ =
      project#set_state
        (Lwt.pick [ Kappa_facade.parse ast overwrites system_process;
                    harakiri >>= fun () ->
                    Lwt.return (Result_util.error
                                  [Api_common.error_msg
                                     "Parse cancelled by modified files"])
                  ]) in
    Lwt.return (Result_util.ok ())

  method simulation_delete : unit Api.result Lwt.t =
    self#simulation_stop >>=
    (fun _ ->
       let () = simulation <- None in
       Lwt.return (Result_util.ok ()))

  method simulation_start
      (simulation_parameter : Api_types_j.simulation_parameter) :
    Api_types_j.simulation_artifact Api.result Lwt.t =
    let (simulation_parameter,simulation_seed) =
      patch_parameter simulation_parameter in
    match simulation with
    | Some _ ->
      Lwt.return
        (Api_common.result_error_msg
           ~result_code:`Conflict "A simulation already exists")
    | None ->
      project#get_state () >>= function
      | None ->
        Lwt.return (Api_common.result_error_msg
                      "Cannot start simulation: Parse not done")
      | Some parse ->
        Result_util.fold
          ~ok:
            (fun (facade : Kappa_facade.t) ->
               (Kappa_facade.start
                  ~system_process
                  ~parameter:simulation_parameter
                  ~t:facade)
               >>=
               (Result_util.fold
                  ~ok:
                    (fun () ->
                       let () =
                         simulation <- Some (simulation_parameter,facade) in
                       Lwt.return
                         (Result_util.ok {
                             Api_types_j.simulation_artifact_simulation_seed = simulation_seed ;
                           }))
                  ~error:
                    (fun errors ->
                       Lwt.return (Api_common.result_messages errors)))
            )
          ~error:
            (fun errors ->
               Lwt.return (Api_common.result_messages errors))
          parse

    method simulation_parameter :
      Api_types_j.simulation_parameter Api.result Lwt.t =
      match simulation with
      | Some (parameter,_) ->
        Lwt.return (Result_util.ok parameter)
      | None ->
        let m  = "No simulation available" in
        Lwt.return (Api_common.result_error_msg ~result_code:`Not_found m)

    method simulation_raw_trace : string Api.result Lwt.t =
      bind_simulation
        simulation
        (fun t ->
           Lwt.return (Result_util.ok
                         (Kappa_facade.get_raw_trace t)))

    method simulation_outputs_zip =
      let projection t =
        try
          let filename = "simulation_outputs" in
          let file = Fakezip.open_out (filename^".zip") in
          let () = Fakezip.add_entry
              t.Api_types_t.simulation_output_inputs
              file (filename^"/inputs.ka") in
          let () = Fakezip.add_entry
              t.Api_types_t.simulation_output_log_messages
              file (filename^"/log.txt") in
          let () =
            match t.Api_types_t.simulation_output_plot with
            | None -> ()
            | Some plot ->
              Fakezip.add_entry
                (Data.export_plot ~is_tsv:false plot)
                file (filename^"/data.csv") in
          let () =
            Mods.StringMap.iter
              (fun name content ->
                 Fakezip.add_entry (String.concat "\n" (List.rev content))
                   file (filename^"/"^name))
              t.Api_types_t.simulation_output_file_lines in
          let () =
            List.iter
              (fun din ->
                 Fakezip.add_entry (Data.string_of_din ?len:None din)
                   file (filename^"/"^din.Data.din_data.Data.din_name))
            t.Api_types_t.simulation_output_dins in
          let () =
            List.iter
              (fun snapshot ->
                 Fakezip.add_entry (Data.string_of_snapshot ?len:None snapshot)
                   file (filename^"/"^snapshot.Data.snapshot_file))
              t.Api_types_t.simulation_output_snapshots in
          let out = Fakezip.close_out file in
          Result_util.ok out
        with Fakezip.Error (_,f,e) ->
          Api_common.result_error_msg ("Zip error in "^f^": "^e) in
      detail_projection ~simulation ~system_process ~projection

    method simulation_pause : unit Api.result Lwt.t =
      bind_simulation
        simulation
        (fun t ->
           (Kappa_facade.pause ~system_process ~t) >>=
           (fun _ ->
             Lwt.return (Result_util.ok ())))

    method private simulation_stop : unit Api.result Lwt.t =
      bind_simulation
        simulation
        (fun t ->
           (Kappa_facade.stop ~system_process ~t) >>=
           (Result_util.fold
              ~ok:((fun () ->
                  Lwt.return (Result_util.ok ())))
              ~error:((fun errors ->
                       Lwt.return (Api_common.result_messages errors)))
           )
        )

    method simulation_intervention
        (simulation_perturbation : Api_types_j.simulation_intervention) :
      string Api.result Lwt.t =
      bind_simulation
        simulation
        (fun t ->
           (Kappa_facade.perturbation
              ~system_process:system_process
              ~t:t
              ~perturbation:simulation_perturbation) >>=
           (Result_util.fold
                ~ok:((fun s -> Lwt.return (Result_util.ok s)))
                ~error:(fun errors ->
                    Lwt.return (Api_common.result_messages errors))
           )
        )

    method simulation_continue (pause_condition : string) :
      unit Api.result Lwt.t =
      bind_simulation
        simulation
        (fun t ->
           (Kappa_facade.continue
              ~system_process:system_process
              ~t:t ~pause_condition) >>=
           (Result_util.fold
                ~ok:((fun () -> Lwt.return (Result_util.ok ())))
                ~error:((fun errors ->
                          Lwt.return (Api_common.result_messages errors)))
           )
        )

    method simulation_info : Api_types_j.simulation_info Api.result Lwt.t =
      bind_simulation
        simulation
        (fun t ->
           Kappa_facade.progress ~system_process ~t >>=
           (Result_util.fold
              ~ok:(fun progress ->
                  Kappa_facade.outputs ~system_process ~t >>=
                  (Result_util.fold
                     ~ok:(fun outputs ->
                         Lwt.return
                           (Result_util.ok
                              (Api_data.api_simulation_status progress outputs))
                       )
                     ~error:(fun errors ->
                         Lwt.return (Api_common.result_messages errors)))
                )
              ~error:(fun errors ->
                  Lwt.return (Api_common.result_messages errors)))
        )

    method simulation_efficiency =
      bind_simulation simulation
        (fun t ->
           Lwt.return (Result_util.ok (Kappa_facade.efficiency t)))

    inherit  manager_file_line system_process
    inherit  manager_flux_map system_process
    inherit  manager_log_message system_process
    inherit  manager_plot system_process
    inherit  manager_snapshot system_process
  end
