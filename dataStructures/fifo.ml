(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type 'a t =
  {
    waiting_elts: 'a list ;
    list: 'a list ;
  }

let empty =
  {
    waiting_elts = [];
    list = []
  }

let is_empty t = t.waiting_elts = [] && t.list = []

let push a t = { t with waiting_elts = a::t.waiting_elts}

let rec pop t =
  match
    t.list
  with
  | head::tail ->
    {t with list = tail},Some head
  | [] ->
    begin
      match t.waiting_elts with
      | [] -> t, None
      | list -> pop {waiting_elts = [] ; list = List.rev list}
    end
