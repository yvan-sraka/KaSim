(******************************************************************************)
(*  _  __ * The Kappa Language                                                *)
(* | |/ / * Copyright 2010-2017 CNRS - Harvard Medical School - INRIA - IRIF  *)
(* | ' /  *********************************************************************)
(* | . \  * This file is distributed under the terms of the                   *)
(* |_|\_\ * GNU Lesser General Public License Version 3                       *)
(******************************************************************************)

open Js
open Codemirror

open Lwt.Infix
let document = Dom_html.window##.document
let has_been_modified = ref (false)

module Html = Tyxml_js.Html5

let file_label_signal, set_file_label = React.S.create ""
let file_label =
  Tyxml_js.R.Html.pcdata
    (React.S.bind file_label_signal (fun env -> React.S.const env))

let save_button_id = "save_button"
let save_button =
  Html.a
    ~a:[ Html.a_id save_button_id
       ; Tyxml_js.R.Html.Unsafe.string_attrib
           "download"
           (React.S.map
              (function
                | None -> ""
                | Some file -> file.Api_types_j.file_metadata.Api_types_j.file_metadata_id
              )
              Ui_state.current_file
           )
       ; Html.Unsafe.string_attrib "role" "button"
       ; Html.a_class ["btn";"btn-default";"pull-right"]
       ]
    [ Html.cdata "save" ]

let toggle_button_id = "toggle_button"
let toggle_button =
  Html.a
    ~a:[ Html.a_id toggle_button_id
       ; Html.Unsafe.string_attrib "role" "button"
       ; Html.a_class ["btn";"btn-default";"pull-right"]
       ]
    [ Html.cdata "toggle" ]

let file_selector =
  Html.input
    ~a:[ Html.a_id "file-selector" ;
         Html.Unsafe.string_attrib "type" "file" ;
         Html.Unsafe.string_attrib "accept" ".ka" ] ()
let panel_heading =
  [%html {|<div class="row">
             <div class="col-md-2">
                <label class="btn btn-default" for="file-selector">
         |}[file_selector]{|
                   Load
                </label>
             </div>
             <div class="col-md-3">
                <label class="filename">|}[file_label]{|</label>
                                                         </div>
                                                         <div class="col-md-2 col-sm-offset-3 pull-right">
                                                      |}[save_button]{|
             </div>
            <div class="col-md-2">
       |}[toggle_button]{|
             </div>
            </div>|}]

let xml (_ : Ui_simulation.t) =
  [Html.div ~a:[Html.a_class ["panel";"panel-default"]]
     [%html {|<div class="panel-heading">
            |}[panel_heading]{|
             </div>
             <div class="panel-body">
                <textarea id="code-mirror"> </textarea>
             </div>|}]]

let error_lint errors : Codemirror.lint Js.t Js.js_array Js.t =
  let position p =
    new%js Codemirror.position (p.Locality.line-1) (p.Locality.chr) in
  let hydrate (error  : Api_types_j.message) : lint Js.t option =
    match error.Api_types_j.message_range with
    | None -> None
    | Some range ->
      Some (Codemirror.create_lint
              ~message:error.Api_types_j.message_text
              (* This is a bit of a hack ... i am trying to keep
                   the code mirror code independent of the api code.
              *)
              ~severity:( match error.Api_types_j.message_severity with
                  | `Error -> Codemirror.Error
                  | `Warning -> Codemirror.Warning
                  | `Info -> Codemirror.Warning
                )
              ~from:(position range.Locality.from_position)
              ~to_:(position range.Locality.to_position)) in
  Js.array
    (Array.of_list
       (List.fold_left
          (fun acc value ->
             match hydrate value with
             | None -> acc
             | Some value -> value::acc)
          []
          errors))


let setup_lint _ _ _ =
  match React.S.value Ui_state.model_error with
  | None -> Js.array [||]
  | Some e ->
    let () =
      Common.debug (Js.string e.Ui_state.model_error_location) in
    let e : Api_types_j.errors = e.Ui_state.model_error_messages in
    error_lint e

let initialize codemirror () =
  let args = Url.Current.arguments in
  let () =
    try Ui_state.set_model_pause_condition (List.assoc "pause" args)
    with Not_found | Failure _ -> () in
  let () =
    try Ui_state.set_model_plot_period
          (float_of_string (List.assoc "plot_period" args))
    with Not_found | Failure _ -> () in
  try
    let url = List.assoc "model" args in
    XmlHttpRequest.get url >>=
    (fun content ->
       if content.XmlHttpRequest.code <> 200 then Lwt.return_unit
       else
         let () =
           match Url.url_of_string content.XmlHttpRequest.url with
           | None -> ()
           | Some u ->
             let filename =
               List_util.last
                 (match u with
                  | (Url.Http h | Url.Https h) -> h.Url.hu_path
                  | Url.File f -> f.Url.fu_path) in
             let filecontent : string =
               content.XmlHttpRequest.content
             in
             let () = Ui_state.set_file filename filecontent in
             let () = codemirror##setValue(Js.string filecontent) in
             ()  in
         Lwt.return_unit)
  with Not_found ->
    let filename = "default.ka" in
    let filecontent =
      try List.assoc "model_text" args
      with Not_found -> ""
    in
    let () = Ui_state.set_file filename filecontent in
    let () = codemirror##setValue(Js.string filecontent) in
    Lwt.return_unit

let onload (t : Ui_simulation.t) : unit =
  let lint_config =
    Codemirror.create_lint_configuration () in
  let () = lint_config##.getAnnotations := setup_lint in
  let () = lint_config##.lintOnChange := Js._false in
  let configuration = Codemirror.default_configuration in
  let gutter_options =
    Js.string "breakpoints,CodeMirror-lint-markers,CodeMirror-linenumbers" in
  let gutter_option : Js.string_array Js.t =
    gutter_options##split(Js.string ",")
  in
  let textarea : Dom_html.element Js.t =
    Js.Opt.get (document##getElementById (Js.string "code-mirror"))
      (fun () -> assert false) in
  let () =
    configuration##.lineNumbers := Js._true;
    configuration##.lineWrapping := Js._true;
    configuration##.styleActiveLine := Js._true;
    configuration##.autofocus := Js._true;
    configuration##.gutters := gutter_option;
    configuration##.lint := lint_config;
    configuration##.mode := (Js.string "Kappa")
  in
  let codemirror : codemirror Js.t =
    Codemirror.fromTextArea textarea configuration in
  let () = codemirror##setValue(Js.string "") in
  let _ =
    React.S.map (fun _ -> codemirror##performLint)
      Ui_state.model_error in
  let _ = Common.async (initialize codemirror) in
  let timeout : Dom_html.timeout_id option ref = ref None in
  let handler = fun codemirror change ->
    let () = has_been_modified := true in
    let text : string = Js.to_string codemirror##getValue in
    let () = match !timeout with
        None -> ()
      | Some timeout ->
        Dom_html.window ## clearTimeout (timeout) in
    let delay : float =
      if (((Js.str_array (change##.text ))##.length) > 1)
         ||
         (Ui_state.has_model_error ())
      then
        1.0 *. 1000.0
      else
        5.0 *. 1000.0
    in
    let handle_timeout () =
      let () = Common.info "handle_timeout" in
      let () = Common.info text in
      Ui_state.set_filecontent
        (Js.to_string codemirror##getValue) in
    let () = timeout := Some
          (Dom_html.window ## setTimeout
             (Js.wrap_callback
                (fun _ -> handle_timeout ())) delay) in
    ()
  in
  let () = codemirror##onChange(handler) in
  let file_select_dom = Tyxml_js.To_dom.of_input file_selector in
  let save_button_dom : Dom_html.linkElement Js.t =
    Js.Unsafe.coerce
      (Js.Opt.get (document##getElementById (Js.string save_button_id))
         (fun () -> assert false)) in
  let toggle_button_dom : Dom_html.linkElement Js.t =
    Js.Unsafe.coerce
      (Js.Opt.get (document##getElementById (Js.string toggle_button_id))
         (fun () -> assert false)) in
  let () =
    save_button_dom##.onclick :=
      Dom.handler
        (fun _ ->
           let header = Js.string "data:text/plain;charset=utf-8," in
           let editor_text :  Js.js_string Js.t = codemirror##getValue in
           let () =
             save_button_dom##.href := header##concat((Js.escape editor_text)) in
           Js._true) in
  let () =
    toggle_button_dom##.onclick :=
      Dom.handler
        (fun _ ->
           let editor_full = React.S.value Ui_state.editor_full in
           let () = Ui_state.set_editor_full (not editor_full) in
           Js._true) in
  let file_select_handler () =
    Js.Optdef.case (file_select_dom##.files)
      (fun () -> Lwt.return_unit)
      (fun files ->
         Js.Opt.case (files##item (0))
           (fun () -> Lwt.return_unit)
           (fun file ->
              let filename = to_string file##.name in
              let () = set_file_label filename in
              let () =
                Common.async
                  ((fun _ ->
                      File.readAsText file >>=
                      (fun  (va : Js.js_string Js.t) ->
                         let () = Ui_state.set_file filename "" in
                         Lwt.return va
                      ) >>=
                      (fun  (va : Js.js_string Js.t) ->
                         let () = codemirror##setValue(va) in
                         Ui_simulation.flush_simulation t)))
              in
              let () = has_been_modified := false in
              Lwt.return_unit)) in
  let confirm () : bool = Js.to_bool
      (Dom_html.window##confirm
         (Js.string "Modifications will be lost, do you wish to continue?"))
  in
  let () =
    Lwt_js_events.async
      (fun () ->
         Lwt_js_events.clicks
           file_select_dom
           (fun _ _ ->
              let () =
                file_select_dom##.value := file_select_dom##.defaultValue in
              Lwt.return_unit)) in
  let ()  =
    Common.async
      (fun () ->
         Lwt_js_events.changes
           file_select_dom
           (fun _ _ ->
              if (not !has_been_modified) || confirm ()
              then file_select_handler ()
              else Lwt.return_unit))
  in ()

let onunload () = ()
