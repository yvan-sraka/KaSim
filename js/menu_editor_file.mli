(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2019 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

val content :
  bool React.signal ->
  [> `Button | `Div | `Ul  | `A of [> `PCDATA | `Span ]] Tyxml_js.Html5.elt list

val onload : unit -> unit
