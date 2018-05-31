(*built a decision tree
WORK IN PROGRESS*)



(*let build_decision_tree parameters error cyclelist ((c_agent_name:list)list)=*)


type 'a non_empty_tree =
  | Node of ('a * ('a non_empty_tree) list)

type 'a tree =
  | Empty
  | Non_empty of 'a non_empty_tree

let rec build_tree n =
  if n<0 then Empty
  else
    let node_label = 2*n+1 in
    let left_potentially_empty_tree = build_tree (n-1) in
    let right_non_empty_tree = Node (2*n+2,[]) in
    match
      left_potentially_empty_tree
    with
    | Empty ->
      Non_empty (Node (node_label,[]))
    | Non_empty left_non_empty_tree ->
      Non_empty (Node (node_label,[left_non_empty_tree;right_non_empty_tree]))

let print_tree fmt to_string tree =
  let rec aux non_empty_tree depth =
    match non_empty_tree with
    | Node (node_label,sibblings_list) ->
      let () = Format.fprintf fmt "%s%s\n" (String.make (8*depth) ' ') (to_string node_label) in
      let () =
        List.iter
          (fun non_empty_tree -> aux non_empty_tree (depth+1))
          (List.rev sibblings_list)
      in
      ()
  in
  match tree with
  | Empty -> Format.fprintf fmt "EMPTY TREE\n"
  | Non_empty non_empty_tree -> aux non_empty_tree 0

let tree = build_tree 3

let () = print_tree Format.std_formatter string_of_int tree

(*let build_decision_tree = ()*)

  let build_decision_tree parameters error mixture
      (allcycle:(((Ckappa_sig.c_site_name  * c_agent_name * Ckappa_sig.c_site_name ) list)list))=
    (** FOR EACH cycle : WORK IN PROGRESS cycle = list of edge ((Ckappa_sig.c_site_name  * c_agent_name * Ckappa_sig.c_site_name ))
        so wrong name  MAKE A CHANGE*)
    (* take the first element of the list and initialize the graph *)
    (* CPT to node ? *)
    (*let for_list liste  = *)
    let cpt = 0 in
    let error, graph = Build_graph.init parameters error handler in

    let n=List.length cyclelis in

    (*  if cpt=0 then
        match cyclelis with
        | (sin,ag,sout)::tail ->
        let error, ag, graph =
          Build_graph.add_agent parameters error ag graph in
        *)

    (* la condition d'arret serait qd il n'y a plus d'element dans la liste ? donc ici  n=list.length *)

    (* FOR BINARY TREES, NEED TO ADAPT FOR OTHER KIND *)
    let rec build_tree graph  cpt n previousag cyclelis=
      (*  if cpt=0 then
          match cyclelis with
          | (sin,ag,sout)::tail ->
          let error, ag, graph =
          Build_graph.add_agent parameters error ag graph in
          (*  add link somewhere....IMPORTANT  *)
          else
      *)
      if n<1 then Empty (*maybe at the end of the list ou maybe 0 because of the cycle?*)
      else
        (*only the first case *)
          if cpt=0 then
            match cyclelis with
            | (sin,ag,sout)::tail ->
              begin
                let error, ag, graph =
                  Build_graph.add_agent parameters error ag graph in
                  let error, graph_free =

                    Build_graph.add_site parameters error cpt 0 graph in
                    let node_label = graph in
            let left_potentially_empty_tree = build_tree (graph cpt+1 n-1 ag tail) in
            let right_non_empty_tree = Node (graph_free,[]) in
            match
              left_potentially_empty_tree
            with
            | Empty ->
              Non_empty (Node (node_label,[]))
            | Non_empty left_non_empty_tree ->
              Non_empty (Node (node_label,[left_non_empty_tree;right_non_empty_tree]))
          end

          (*  everything exept first case  *)
          else
            match cyclelis with
            | (sin,ag,sout)::tail ->
          (*let error, graph =
            Build_graph.add_link parameters error ag_id st ag_id' st' graph*)
              begin
              let error, ag, graph =
                Build_graph.add_agent parameters error ag graph in
              (*we need agent_id and not agent_name , so we use cpt for agent_id*)



              let error, graph =
                Build_graph.add_link parameters error previousag sin ag sout graph
              in
(*graph_free= the site is free*)

              let error, ag_id_site, graph_free =

                Build_graph.add_site parameters error cpt 0 graph in

              let node_label = graph in
              let left_potentially_empty_tree = build_tree (graph cpt+1 n-1 ag tail) in
              let right_non_empty_tree = Node (graph_free,[]) in
              match
                left_potentially_empty_tree
              with
              | Empty ->
                Non_empty (Node (node_label,[]))
              | Non_empty left_non_empty_tree ->
                Non_empty (Node (node_label,[left_non_empty_tree;right_non_empty_tree]))
            end
    in
    (*end loop for list *)
