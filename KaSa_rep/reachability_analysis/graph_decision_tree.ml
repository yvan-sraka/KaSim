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
      let () =
        Loggers.print_newline
          (Remanent_parameters.get_logger parameters)
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

         let error, graph = Build_graph.init parameters error handler in

         match
           cyclelis
         with
         | [] -> error, list
         | (first_agent_incoming_site, first_agent_name, first_agent_outcoming_site)::tail
           ->
           let error, first_agent_id, graph =
             Build_graph.add_agent
               parameters error
               first_agent_name
               graph
           in
           let rec
             build_dtree
               error
               graph nodelist
               previous_agent_id previous_agent_outcoming_site
               first_agent_id first_agent_type first_agent_incoming_site =
             match nodelist with
             | [] ->
               begin
                 let node_label = graph in
                 let error, graph =
                   Build_graph.add_site
                     parameters error previous_agent_id previous_agent_outcoming_site graph
                 in
                 (*let error, graph =
                       Build_graph.add_site parameters error ag sout graph in*)

                 (*graph_free= the site is free*)
                 let error, graph_free =
                   Build_graph.add_free
                     parameters error
                     previous_agent_id
                     previous_agent_outcoming_site
                     graph
                 in

                 let nonbinded_site_leaf = Node (graph_free,[]) in

                 (**)
                 let error, graph_cycle =
                   Build_graph.add_site
                     parameters error
                     first_agent_id first_agent_incoming_site
                     graph
                 in

                 let error, graph_cycle =
                   Build_graph.add_link
                     parameters error
                     previous_agent_id previous_agent_outcoming_site
                     first_agent_id first_agent_incoming_site
                     graph_cycle
                 in

                 let error, fresh_agent_id, graph_fresh_agent =
                   Build_graph.add_agent
                     parameters error
                     first_agent_type
                     graph
                 in
                 let error, graph_fresh_agent =
                   Build_graph.add_site
                     parameters error
                     fresh_agent_id first_agent_incoming_site
                     graph_fresh_agent
                 in

                 let error, graph_fresh_agent =
                   Build_graph.add_link
                     parameters error
                     previous_agent_id previous_agent_outcoming_site
                     fresh_agent_id first_agent_incoming_site
                     graph_fresh_agent
                 in
                 let repeat_agent_leaf=
                   Node (graph_fresh_agent,[])
                 in
                 let cycle_leaf = Node(graph_cycle,[])
                 in
                 error,
                 Non_empty (Node (node_label,[cycle_leaf;repeat_agent_leaf;nonbinded_site_leaf]))
               end
               (*second case*)
             | (next_agent_incoming_site,
                next_agent_name,
                next_agent_outcoming_site)::tail ->
               begin
                 let node_label = graph in
                 let error, graph =
                   Build_graph.add_site
                     parameters error
                     previous_agent_id
                     previous_agent_outcoming_site
                     graph
                 in
                 let error, graph_free =
                   Build_graph.add_free
                     parameters error
                     previous_agent_id
                     previous_agent_outcoming_site
                     graph
                 in
                 let error, next_agent_id, graph =
                   Build_graph.add_agent
                     parameters error next_agent_name graph
                 in
                 let error, graph_fresh_agent =
                     Build_graph.add_site
                       parameters error
                       next_agent_id
                       next_agent_incoming_site  graph
                 in
                 let error, graph_fresh_agent =
                   Build_graph.add_link
                     parameters error
                     previous_agent_id previous_agent_outcoming_site
                     next_agent_id next_agent_incoming_site
                     graph_fresh_agent
                 in
                 let error, left_potentially_empty_tree =
                   build_dtree
                     error
                     graph_fresh_agent tail
                     next_agent_id next_agent_outcoming_site
                     first_agent_id first_agent_type first_agent_incoming_site
                 in
                 let right_non_empty_tree = Node (graph_free,[]) in
                 match
                   left_potentially_empty_tree
                 with
                 | Empty ->
                   Exception.warn parameters error __POS__ Exit
                     (Non_empty (Node (node_label,[right_non_empty_tree])))
                 | Non_empty left_non_empty_tree ->
                   error,
                   Non_empty (Node (node_label,[left_non_empty_tree;right_non_empty_tree]))
               end
         in
         (*previous agent should be null  VERIFY*)
         let error, tree =
           build_dtree
             error
             graph tail
             first_agent_id
             first_agent_outcoming_site
             first_agent_id
             first_agent_name
             first_agent_incoming_site

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
   decision tree for each cycle
 ********************************)
(* test function : run if local_trace = true *)

let decision_trees_for_each_cycle parameters error handler cycle =
  let lis = List.map (fun c -> let error,tree = build_decision_tree_list parameters error handler c in tree)
      cycle
  in error,lis


(********************************
   FUNCTION PRINT
 ********************************)

let print parameters error tree =
  print_tree
    parameters error
    Build_graph.print tree


(********************************
   FUNCTION PRINT ALL
 ********************************)
(* test function : run if local_trace = true *)
(*let print_all_tree parameters error handler cycles =

  let error, tree_l = build_decision_tree_list parameters error handler cycles in*)
let print_all_tree parameters error handler tree_l=
  let ()= Loggers.fprintf (Remanent_parameters.get_logger parameters)
      "PRINT TREE BEGINNING \n"
  in

    let error = List.fold_left  (fun error c ->
        let () = Loggers.fprintf (Remanent_parameters.get_logger parameters)
            "new trees\n "
        in
        List.fold_left  (fun error c ->   let error = print
                                parameters
                                error
                                c
in
                                let () = Loggers.fprintf (Remanent_parameters.get_logger parameters)
                                    "\n \n"

in error)
          error c
      ) error tree_l
    in  error

  (*let print_all_tree parameters error handler cycles =

    let error, tree_l = build_decision_tree_list parameters error handler cycles in
  (*let print_all_tree parameters error handler tree_l=*)

    let () = List.iter (fun c ->
        let () = Loggers.fprintf (Remanent_parameters.get_logger parameters)
            "\n"
        in
        List.iter (fun c -> let error = print
                                parameters
                                error
                                c
                    in ()) c
      )tree_l
    in  error*)



(********************************
   FUNCTION TEST
 ********************************)
(* test function : run if local_trace = true *)


(*let main parameters error handler =
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
    error, de_tree*)



  (*let print parameters error tree =
    print_tree
      parameters error
      Build_graph.print tree*)
