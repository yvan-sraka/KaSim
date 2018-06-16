let local_trace = true
(*build a decision tree *)


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

 let print_tree parameters error print_node_label tree =
  let rec aux parameters non_empty_tree depth error =
    match non_empty_tree with
    | Node (node_label,sibblings_list) ->
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "%s" (String.make (8*depth) ' ')
      in
      let error =
        print_node_label
          parameters error
          node_label
      in
      let error =
        List.fold_left
          (fun error non_empty_tree ->
             aux parameters non_empty_tree (depth+1) error)
          error (List.rev sibblings_list)
      in
      error
  in
  match tree with
  | Empty ->
    let () =
      Loggers.fprintf
        (Remanent_parameters.get_logger parameters) "EMPTY TREE"
    in
    let () =
      Loggers.print_newline (Remanent_parameters.get_logger parameters)
    in
    error
  | Non_empty non_empty_tree -> aux parameters non_empty_tree 0 error

  (*let tree = build_tree 3

  let () = print_tree Format.std_formatter string_of_int tree*)

(*let build_decision_tree = ()*)



let decision_tree_list parameters error handler (allcycle:(((Ckappa_sig.c_site_name  * Ckappa_sig.c_agent_name * Ckappa_sig.c_site_name ) list)list))=
  (** FOR EACH cycle : WORK IN PROGRESS cycle = list of edge ((Ckappa_sig.c_site_name  * c_agent_name * Ckappa_sig.c_site_name ))
  *)
  (* take the first element of the list and initialize the graph *)
  (* CPT to node ?  *)
  (*let for_list liste  = *)
  (************ PRINT **********)

  let ()= Loggers.fprintf (Remanent_parameters.get_logger parameters)
      "START DECISION TREE \n"
  in

  (************ END PRINT ************)

  let error, trees =
    List.fold_left
      (fun (error,list) cyclelis ->
         (*we need agent_id and not agent_name , so we use cpt for agent_id*)
         let cpt = (Ckappa_sig.agent_id_of_int 0 ) in
         let error, graph = Build_graph.init parameters error handler in

         let n=List.length cyclelis in
         let sinH,head,southH  = List.hd cyclelis in
         (*need also head in out*)
         (**********************)
         let rec build_dtree graph cpt n previousag last_site cyclelis sinH head southH=

           if n=1 then
             (*stop case*)
             match cyclelis with
             | [] -> Empty
             | (sin,ag_name,sout)::tail ->
               begin

                 let error, ag, graph =
                   Build_graph.add_agent parameters error ag_name graph in

                   let error, graph =
                     Build_graph.add_site parameters error ag sin graph in


                 let error, graph =
                   Build_graph.add_link parameters error previousag last_site ag sin graph
                 in

                 (*graph_free= the site is free*)
                 let error, graph_site = Build_graph.add_site parameters error ag sout graph in

                 let error, graph_free = Build_graph.add_free parameters error ag sout graph_site in

                 (*be sure last_site = sout*)
                 let last_site = sout in
                 let cpt= Ckappa_sig.agent_id_of_int ((Ckappa_sig.int_of_agent_id cpt)+1) in
                 let node_label = graph in
                 (*leaf*)
                 let nonbinded_site_leaf = Node (graph_free,[]) in

                 (**)
                 let error, head, graph =
                   Build_graph.add_agent parameters error head graph_site in

                 let error, graph = Build_graph.add_site parameters error head sinH graph in

                 let error, graphl =
                   Build_graph.add_link parameters error ag last_site head sinH graph
                 in

                 let node_labell = graphl in
                 (*we add the link for the cycle*)

                 let error, graphcy =
                   Build_graph.add_link parameters error ag last_site head
                     sinH graph_site
                 in

                 let node_labelcy = graphcy in



                 let repeat_agent_leaf=
                   Node (node_labell,[])
                 in
                 let cycle_leaf = Node(node_labelcy,[])
                 in
                 Non_empty (Node (node_label,[cycle_leaf;repeat_agent_leaf;nonbinded_site_leaf]))
               end
               (*second case*)
           else
             (* only the first case : peut etre pas besoin ? *)
           if (Ckappa_sig.int_of_agent_id cpt)=0 then
             match cyclelis with
             | [] -> Empty
             | (sin,ag,sout)::tail ->
               begin
                 let error, ag, graph =
                   Build_graph.add_agent parameters error ag graph in
                 (************ PRINT **********)

                 let ()= Loggers.fprintf (Remanent_parameters.get_logger parameters)
                     " FIRST CASE : %i \n" (Ckappa_sig.int_of_agent_id ag)
                 in

                 (************ END PRINT ************)


                 let error, graph =

                   Build_graph.add_site parameters error ag
                     (*SOUT? NO SURE *)
                     sout
                     graph in

                 let error, graph_free = Build_graph.add_free parameters error ag
                     (*SOUT? NO SURE *)
                     sout graph
                 in
                 (************ PRINT **********)

                 let ()= Loggers.fprintf (Remanent_parameters.get_logger parameters)
                     "        D : %i -[ \n" (Ckappa_sig.int_of_agent_id ag)
                 in

                 (************ END PRINT ************)


                 let node_label = graph in

                 let cpt= Ckappa_sig.agent_id_of_int ((Ckappa_sig.int_of_agent_id cpt)+1) in
                 (*be sure last_site = sout?*)
                 let last_site = sout in
                 let left_potentially_empty_tree =
                   build_dtree graph
                     cpt
                     (n-1)
                     ag
                     last_site
                     tail sinH head southH in

                 let right_non_empty_tree = Node (graph_free,[]) in
                 match
                   left_potentially_empty_tree
                 with
                 | Empty ->
                   Non_empty (Node (node_label,[]))
                 | Non_empty left_non_empty_tree ->
                   Non_empty (Node (node_label,[left_non_empty_tree;right_non_empty_tree]))
               end

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

                 let error, graph_site =

                   Build_graph.add_site
                     parameters error
                     ag
                     (*SOUT? NO SURE *)
                     sin graph in

                     let error, graph_site =

                       Build_graph.add_site
                         parameters error
                         ag
                         (*SOUT? NO SURE *)
                         sout graph in

                 let error, graph =
                   Build_graph.add_link parameters error previousag last_site ag sin graph_site
                 in
                 (*graph_free= the site is free*)

                 (************ PRINT **********)

                 let ()= Loggers.fprintf (Remanent_parameters.get_logger parameters)
                     " G : %i - %i \n" (Ckappa_sig.int_of_agent_id previousag) (Ckappa_sig.int_of_agent_id ag)
                 in

                 (************ END PRINT ************)

                 let error, graph_free = Build_graph.add_free parameters error cpt
                     (*SOUT? NO SURE *)
                     sout graph_site

                 in
                 (************ PRINT **********)

                 let ()= Loggers.fprintf (Remanent_parameters.get_logger parameters)
                     " D :%i - %i -[ \n" (Ckappa_sig.int_of_agent_id previousag) (Ckappa_sig.int_of_agent_id ag)
                 in

                 (************ END PRINT ************)


                 let last_site = sout in
                 let cpt= Ckappa_sig.agent_id_of_int ((Ckappa_sig.int_of_agent_id cpt)+1) in
                 let node_label = graph in
                 let left_potentially_empty_tree = build_dtree graph cpt (n-1) ag last_site tail sinH head southH in
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
         (*previous agent should be null  VERIFY*)
         let tree =
           build_dtree graph cpt n Ckappa_sig.dummy_agent_id Ckappa_sig.dummy_site_name cyclelis sinH head southH

         in
         error, tree::list

      )
      (error,[])
      (List.rev allcycle)
  in
  error, trees
(*end loop for list *)

(********************************
   decision tree permutation
 ********************************)

let circularpermut cycle =
  let rec aux prefix_rev suffix resultat =
    match suffix with
    | [] -> resultat
    | head::suffix ->
      let prefix_rev = head::prefix_rev in
      let list =
        List.fold_left
          (fun list elt -> elt::list) (List.rev prefix_rev) (List.rev suffix)
      in
      aux prefix_rev suffix (list::resultat)
  in
  aux [] cycle []

(********************************
   call of tree function after permut and for each cycle
 ********************************)
(* -apel circular permut pour chaque list
   - flatter les listes
- apel arbre decision tree*)

let build_decision_tree_list parameters error handler allcycle =
  let lis = List.map (fun c-> circularpermut c)allcycle

  in
  let cyclec = List.flatten lis

  in decision_tree_list parameters error handler cyclec


(********************************
   FUNCTION TEST
 ********************************)
(* test function : run if local_trace = true *)

let main parameters error handler =
  let ()= Loggers.fprintf (Remanent_parameters.get_logger parameters)
      "START MAIN \n"
  in
  let () = Loggers.print_newline (Remanent_parameters.get_logger parameters) in
  let allcycles = [[(Ckappa_sig.site_name_of_int 0, Ckappa_sig.agent_name_of_int 1, Ckappa_sig.site_name_of_int 1);
                    (Ckappa_sig.site_name_of_int 2, Ckappa_sig.agent_name_of_int 2, Ckappa_sig.site_name_of_int 3);
                    (Ckappa_sig.site_name_of_int 4, Ckappa_sig.agent_name_of_int 3, Ckappa_sig.site_name_of_int 5);
                    (Ckappa_sig.site_name_of_int 5, Ckappa_sig.agent_name_of_int 4, Ckappa_sig.site_name_of_int 6);
                   ];
                   [(Ckappa_sig.site_name_of_int 0, Ckappa_sig.agent_name_of_int 1, Ckappa_sig.site_name_of_int 1);
                    (Ckappa_sig.site_name_of_int 2, Ckappa_sig.agent_name_of_int 2, Ckappa_sig.site_name_of_int 3);
                    (Ckappa_sig.site_name_of_int 4, Ckappa_sig.agent_name_of_int 3, Ckappa_sig.site_name_of_int 5);
                   ];
                   [(Ckappa_sig.site_name_of_int 0, Ckappa_sig.agent_name_of_int 1, Ckappa_sig.site_name_of_int 1);
                    (Ckappa_sig.site_name_of_int 2, Ckappa_sig.agent_name_of_int 2, Ckappa_sig.site_name_of_int 3);
                    (Ckappa_sig.site_name_of_int 4, Ckappa_sig.agent_name_of_int 3, Ckappa_sig.site_name_of_int 5);
                    (Ckappa_sig.site_name_of_int 5, Ckappa_sig.agent_name_of_int 4, Ckappa_sig.site_name_of_int 6);]
                  ]
  in
  let de_tree = build_decision_tree_list parameters error handler allcycles in
  error, de_tree

let print parameters error tree =
  print_tree
    parameters error
    Build_graph.print tree
