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

let build_decision_tree parameters error handler mixture
    (allcycle:(((Ckappa_sig.c_site_name  * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name ) list)list))=
  (** FOR EACH cycle : WORK IN PROGRESS cycle = list of edge ((Ckappa_sig.c_site_name  * c_agent_name * Ckappa_sig.c_site_name ))
      so wrong name  MAKE A CHANGE*)
  (* take the first element of the list and initialize the graph *)
  (* CPT to node ? *)
  (*let for_list liste  = *)

  let error, trees =
    List.fold_left
      (fun (error, cyclelis)->
         let cpt = (Ckappa_sig.agent_id_of_int 0 ) in
         let error, graph = Build_graph.init parameters error handler in

         let n=List.length cyclelis in
         let head = List.hd cyclelis in
         (*need also head in out*)
         (*****************************************)
         let tree cyclelis =
           match cyclelis with
           | [] -> Empty
           | (sinH,head,soutH)::tail ->

             let error, head, graph =
               Build_graph.add_agent parameters error head graph in
             let error, graph_free =

               Build_graph.add_site parameters error cpt
                 (*SOUT? NO SURE *)
                 soutH
                 graph in
             let node_label = graph in

             let cpt= Ckappa_sig.agent_id_of_int ((Ckappa_sig.int_of_agent_id cpt)+1) in

             let left_potentially_empty_tree =
               build_dtree graph
                 cpt
                 (n-1)
                 ag
                 tail in
             let right_non_empty_tree = Node (graph_free,[]) in
             match
               left_potentially_empty_tree
             with
             | Empty ->
               Non_empty (Node (node_label,[]))
             | Non_empty left_non_empty_tree ->
               Non_empty (Node (node_label,[left_non_empty_tree;right_non_empty_tree]))

         (**********************)
         in
         (* problem with  *)
         let rec build_dtree graph cpt n previousag cyclelis =
           (*  if cpt=0 then
               match cyclelis with
               | (sin,ag,sout)::tail ->
               let error, ag, graph =
               Build_graph.add_agent parameters error ag graph in
               (*  add link somewhere....IMPORTANT  *)
               else
           *)
           if n<1 then
             (*stop case : at the end of the list : two cases : cycle or no cycle*)
             (*first case*)
             begin
               let error, head, graph =
                 Build_graph.add_agent parameters error head graph in
               (*we need agent_id and not agent_name , so we use cpt for agent_id*)

               let error, graphl =
                 Build_graph.add_link parameters error previousag sinH ag soutH graphl
               in

               let node_label = graphl in
               (*we add the link for the cycle*)

               let error, graphcy =
                 Build_graph.add_link parameters error previousag sin head sout graphl
               in

               let node_labelcy = graphcy in
               Non_empty (Node (node_label,node_labelcy))

             end
             (*second case*)
             (*only the first case *)
             (*if (Ckappa_sig.int_of_agent_id cpt)=0 then
               match cyclelis with
               | [] -> Empty
               | (sin,ag,sout)::tail ->
                 begin
                   let error, ag, graph =
                     Build_graph.add_agent parameters error ag graph in
                   let error, graph_free =

                     Build_graph.add_site parameters error cpt
                       (*SOUT? NO SURE *)
                       sout
                       graph in
                   let node_label = graph in

                   let cpt= Ckappa_sig.agent_id_of_int ((Ckappa_sig.int_of_agent_id cpt)+1) in

                   let left_potentially_empty_tree =
                     build_dtree graph
                       cpt
                       (n-1)
                       ag
                       tail in
                   let right_non_empty_tree = Node (graph_free,[]) in
                   match
                     left_potentially_empty_tree
                   with
                   | Empty ->
                     Non_empty (Node (node_label,[]))
                   | Non_empty left_non_empty_tree ->
                     Non_empty (Node (node_label,[left_non_empty_tree;right_non_empty_tree]))
                 end*)

           (*  everything except first case  *)
           else
             match cyclelis with
             | [] -> Empty
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

                 let error, graph_free =

                   Build_graph.add_site
                     parameters error
                     cpt
                     (*SOUT? NO SURE *)
                     sout graph in
                 let cpt= Ckappa_sig.agent_id_of_int ((Ckappa_sig.int_of_agent_id cpt)+1) in
                 let node_label = graph in
                 let left_potentially_empty_tree = build_dtree graph cpt (n-1) ag tail in
                 let right_non_empty_tree = Node (graph_free,[]) in
                 match
                   left_potentially_empty_tree
                 with
                 | Empty ->
                   Non_empty (Node (node_label,[]))
                 | Non_empty left_non_empty_tree ->
                   Non_empty (Node (node_label,[left_non_empty_tree;right_non_empty_tree]))
               end
               (* in
                  (*previous agent needs to be nothing PROBLEM*)
                  let tree =
                  build_dtree graph cpt n (Ckappa_sig.agent_id_of_int 0) cyclelis
                  in tree)*)
         in tree )

      error
      (List.rev allcycle)
  in
  error, trees
(*end loop for list *)
