(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

type directive_unit = Time | Event

type t = {
  mutable alg_var_overwrite   : (string * Nbr.t) list;
  mutable marshalizedInFile   : string;
  mutable initialMix          : string option;
  mutable rescale             : float option;
  mutable seedValue : int option;
  mutable unit : directive_unit;
  mutable marshalizeOutFile : string option;
  mutable domainOutputFile : string option;
  mutable traceFile : string option;
  mutable eclipseMode : bool;
  mutable compileMode : bool;
  mutable maxSharing : bool;
}

let default : t = {
  alg_var_overwrite = [];
  rescale = None;
  marshalizedInFile = "";
  initialMix = None;
  seedValue  = None;
  unit = Time;
  marshalizeOutFile = None;
  domainOutputFile = None;
  traceFile = None;
  eclipseMode = false;
  compileMode = false;
  maxSharing = false;
}

let options (t :t)  : (string * Arg.spec * string) list = [
(*    ("-mixture",
       Arg.String (fun fic -> t.initialMix <- Some fic),
       "Take the initial state from this file (ignore %init from other files)") ;*)
  ("-var",
   Arg.Tuple
     (let tmp_var_name = ref "" in
      [Arg.String (fun name -> tmp_var_name := name);
       Arg.String (fun var_val ->
           t.alg_var_overwrite <-
             (!tmp_var_name,
              try Nbr.of_string var_val with
                Failure _ ->
                raise (Arg.Bad ("\""^var_val^"\" is not a valid value")))
             ::t.alg_var_overwrite)]),
   "Set a variable to a given value") ;
  ("-load-sim",
   Arg.String (fun file -> t.marshalizedInFile <- file),
   "load simulation package instead of kappa files");
  ("-rescale",
   Arg.Float (fun i -> t.rescale <- Some i),
   "Apply rescaling factor to initial condition");
  ("-u",
   Arg.String
     (function
       | "time" | "Time" | "t" -> t.unit <- Time
       | "event" | "events" | "e" | "Event" | "Events" -> t.unit <-
           Event
       | s -> raise (Arg.Bad ("Unrecognized unit: "^s))),
   "unit (time/event) in which limit and plot period are specified");
  ("-e",
   Arg.Int (fun e ->
       raise (Arg.Bad ("Option '-e' has been replace by '-u event -l "^
                       string_of_int e^"'"))),"Deprecated option");
  ("-make-sim",
   Arg.String
     (fun marshalizeOutFile -> t.marshalizeOutFile <- Some marshalizeOutFile),
   "save kappa files as a simulation package") ;
  ("-dump-cc",
   Arg.String
     (fun domainOutputFile -> t.domainOutputFile <- Some domainOutputFile),
   "file name for dumping the domain of observables") ;
  ("-trace",
   Arg.String
     (fun traceFile -> t.traceFile <- Some traceFile),
   "file name for dumping the simulation trace") ;
  ("-seed", Arg.Int (fun i -> t.seedValue <- Some i),
   "Seed for the random number generator") ;
  ("--eclipse",
   Arg.Unit (fun () -> t.eclipseMode <- true),
   "enable this flag for running KaSim behind eclipse plugin") ;
  ("--max-sharing",
   Arg.Unit (fun () -> t.maxSharing <- true),
   "Initialization is heavier but simulation is faster");
  ("--compile",
   Arg.Unit (fun () -> t.compileMode <- true),
   "Display rule compilation as action list") ;
  ]
