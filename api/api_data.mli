(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type simulation_detail_output =
  (Api_types_t.plot option,
   Api_types_t.din list,
   string list Mods.StringMap.t,
   Api_types_t.snapshot list,
   string,
   string)
    Api_types_t.simulation_output

val api_message_errors :
  ?severity:Api_types_t.severity ->
  ?region:Api_types_t.range -> string -> Api_types_t.message
val api_exception_errors : exn -> Api_types_t.errors
val api_snapshot_dot : Api_types_t.snapshot -> string
val api_snapshot_kappa : Api_types_t.snapshot -> string
val api_simulation_status :
  Api_types_t.simulation_progress -> simulation_detail_output ->
  Api_types_t.simulation_info
val agent_count : Api_types_t.site_graph -> int
