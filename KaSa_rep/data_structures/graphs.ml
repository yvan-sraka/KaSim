let local_trace = false

type node = int

let node_of_int x = x
let int_of_node (x:node) = (x:int)

module NodeSetMap =
  (SetMap.Make
       (struct
         type t = node
         let compare = compare
         let print = Format.pp_print_int
       end
       ))
module NodeMap = NodeSetMap.Map

module Fixed_size_array =
  (
    Int_storage.Quick_key_list(Int_storage.Int_storage_imperatif) :
      Int_storage.Storage
    with type key = node
     and type dimension = int
  )

module Nodearray =
  (
    Int_storage.Nearly_inf_Imperatif :
      Int_storage.Storage
    with type key = node
     and type dimension = int
  )

type ('node_labels,'edge_labels) graph =
  {
    node_labels: 'node_labels Fixed_size_array.t ;
    edges: (node * 'edge_labels) list Fixed_size_array.t ;
  }

let create parameters error node_of_node_label node_list edge_list =
  let max_node =
    List.fold_left
      (fun m i -> max m (int_of_node i))
      0 node_list
  in
  let error, nodes = Fixed_size_array.create parameters error max_node  in
  let error, nodes =
    List.fold_left
      (fun (error, nodes) i ->
         Fixed_size_array.set parameters error (i:node) (node_of_node_label i) nodes)
      (error, nodes)
      (List.rev node_list)
  in
  let error, edges = Fixed_size_array.create parameters error max_node in
  let add_edge parameters error (n1,label,n2) edges =
    let error, old  =
      match Fixed_size_array.unsafe_get parameters error n1 edges with
      | error, None -> error, []
      | error, Some a -> error, a
    in
    Fixed_size_array.set parameters error n1 ((n2,label)::old) edges
  in
  let error, edges =
    List.fold_left
      (fun (error,edges) edge ->
         add_edge parameters error edge edges)
      (error, edges)
      edge_list
  in
  {
    node_labels = nodes ;
    edges =  edges ;
  }

let get parameters error i t =
  match Nodearray.unsafe_get parameters error i t
  with error, Some i -> error, i
     | error, None -> error, -1

let get_b parameters error i t =
  match Nodearray.unsafe_get parameters error i t
  with error, Some i -> error, i
     | error, None -> error, false

let compute_scc
    ?low ?pre ?on_stack
    parameters error n_to_string graph =
  let error =
    if local_trace || Remanent_parameters.get_trace parameters
    then
      let () =
        Loggers.fprintf
          (Remanent_parameters.get_logger parameters)
          "COMPUTE BRIDGE: \n Graph: \n Nodes: \n"
      in
      let error =
        Fixed_size_array.iter
          parameters error
          (fun parameters error i j ->
             let () =
               Loggers.fprintf
               (Remanent_parameters.get_logger parameters)
               "%i %s;\n"
               i (n_to_string j) in
             error)
          graph.node_labels
      in
      let error =
        Fixed_size_array.iter
          parameters error
          (fun parameters error i l ->
             let () =
               Loggers.fprintf
                 (Remanent_parameters.get_logger parameters)
                 "%i:" i
              in
             let error =
               List.fold_left
                 (fun error (j,_) ->
                    let () =
                      Loggers.fprintf
                        (Remanent_parameters.get_logger parameters)
                        "%i," j
                    in
                    error)
                 error
                 l
             in
             let () =
               Loggers.print_newline
                 (Remanent_parameters.get_logger parameters)
             in
             error)
          graph.edges
      in
      error
    else
      error
  in
  let error, low, pre, on_stack =
    match low, pre, on_stack with
    | Some low, Some pre, Some on_stack ->
      error, low, pre, on_stack
    | None, _, _| _, None, _| _, _, None ->
      let error, max_node =
        Fixed_size_array.fold
          parameters error
          (fun _parameter error i _ j -> error, max (int_of_node i) j)
          graph.node_labels
              0
      in
      let error, low =
        match low with
        | Some low -> error, low
        | None -> Nodearray.create parameters error 1
      in
      let error, pre =
        match pre with
        | Some pre -> error, pre
        | None -> Nodearray.create parameters error 1
      in
      let error, on_stack =
        match on_stack with
        | Some on_stack -> error, on_stack
        | None -> Nodearray.create parameters error 1
      in
      error, low, pre, on_stack
  in
  let rec aux parameters error pre low on_stack scc_list stack counter v =
    let error, pre  =
      Nodearray.set parameters error v counter pre
    in
    let error, low  =
      Nodearray.set parameters error v counter low
    in
    let stack = v::stack in
    let error, on_stack =
      Nodearray.set parameters error v true on_stack
    in
    let counter = succ counter in
    let error, edges_v =
      match Fixed_size_array.unsafe_get parameters error v graph.edges with
      | error, None ->
        error, []
      | error, Some a ->
        error, a
    in
    let error, (pre, low, counter, on_stack, scc_list, stack) =
      List.fold_left
        (fun
          (error, (pre, low, counter, on_stack, scc_list, stack))
          (w,_) ->
          let error, pre_w = get parameters error w pre in
          let error, (pre, low, counter, on_stack, scc_list, stack) =
            if pre_w = -1
            then
              let error, (pre, low, counter, on_stack, scc_list, stack) =
                aux parameters error pre low on_stack scc_list stack counter w
              in
              let error, low_v = get parameters error v low in
              let error, low_w = get parameters error w low in
              let error, low =
                Nodearray.set parameters error v
                  (min low_v low_w) low
              in
              error, (pre, low, counter, on_stack, scc_list, stack)
            else
              let error, b  = get_b parameters error w on_stack in
              if b then
                let error, low_v = get parameters error v low in
                let error, pre_w = get parameters error w pre in
                let error, low =
                  Nodearray.set parameters error v (min low_v pre_w) low
                in
                error, (pre, low, counter, on_stack, scc_list, stack)
              else
                error, (pre, low, counter, on_stack, scc_list, stack)
          in
          error, (pre, low, counter, on_stack, scc_list, stack))
        (error, (pre, low, counter, on_stack, scc_list, stack))
        edges_v
    in
    let error, low_v = get parameters error v low in
    let error, pre_v = get parameters error v pre in
    if low_v = pre_v
    then
      let rec aux2 parameters error pre low on_stack scc_list stack counter cc v
        =
        match stack with
        | w'::stack ->
          let error, on_stack =
            Nodearray.set parameters error w' false on_stack
          in
          let cc = w'::cc in
          if v=w' then
            error, (pre, low, counter, on_stack, cc::scc_list, stack)
          else
            aux2
              parameters error pre low on_stack scc_list stack counter cc
              v
        | [] -> assert false
      in
      aux2 parameters error pre low on_stack scc_list stack counter [] v
    else
      error, (pre, low, counter, on_stack, scc_list, stack)
  in
  let error, (pre, low, counter, on_stack, scc_list, stack) =
    Fixed_size_array.fold
      parameters error
      (fun parameters error  v _ ( pre, low, counter, on_stack, scc_list, stack) ->
         let error, pre_v = get parameters error v pre in
         if pre_v = -1 then
           aux parameters error pre low on_stack scc_list stack counter v
         else
           error, (pre, low, counter, on_stack, scc_list, stack))
      graph.node_labels
      (pre, low, 1, on_stack, [], [])
  in
  let () =
    if local_trace ||
       Remanent_parameters.get_trace parameters
    then
      let () =
        Loggers.fprintf (Remanent_parameters.get_logger parameters) "SCC"
      in
      let _ =
        List.iter
          (fun list  ->
             let () =
               List.iter
                 (Loggers.fprintf
                   (Remanent_parameters.get_logger parameters)
                   "%i;")
               list in
             let () =
               Loggers.fprintf
                 (Remanent_parameters.get_logger parameters)
                 "\n"
             in
             ())
          scc_list
      in
      Loggers.print_newline
        (Remanent_parameters.get_logger parameters)
  in
  let error, pre = Nodearray.free_all parameters error pre in
  let error, low = Nodearray.free_all parameters error low in
  let error, on_stack = Nodearray.free_all parameters error on_stack in
  error, pre, low, on_stack, scc_list

let detect_bridges
    parameters error
    add graph string_of_n string_of_e scc bridges =
  Fixed_size_array.fold
    parameters
    error
    (fun parameters error ni l bridges ->
         let error, scci =
           match Nodearray.get parameters error ni scc
           with
           | error, Some scci -> error, scci
           | error, None ->
             Exception.warn parameters error __POS__ Exit (-1)
         in
         List.fold_left
           (fun (error, bridges) (nj,label) ->
              let error, sccj =
                match Nodearray.get parameters error nj scc
                with
                | error, Some sccj -> error, sccj
                | error, None ->
                  Exception.warn parameters error __POS__ Exit (-2)
              in
              if scci=sccj
              then error, bridges
              else
                match
                  Fixed_size_array.get parameters error ni graph.node_labels
              with
              | erro, None ->
                Exception.warn parameters error __POS__ Exit bridges
              | error, Some nstringi ->
                begin
                  match
                    Fixed_size_array.get parameters error nj graph.node_labels
                  with
                  | error, None ->
                  Exception.warn parameters error __POS__ Exit bridges
                  | error, Some nstringj  ->
                    let () =
                      if Remanent_parameters.get_trace parameters || local_trace
                      then
                        let () =
                          Loggers.fprintf
                            (Remanent_parameters.get_logger parameters)
                            "%s %s %s"
                            (string_of_n nstringi)
                            (string_of_e label)
                            (string_of_n nstringj)
                        in
                        Loggers.print_newline
                          (Remanent_parameters.get_logger parameters)
                    in
                    error,add (nstringi,label,nstringj) bridges
                end)
           (error, bridges) l)
      graph.edges
      bridges

let add_bridges
    ?low ?pre ?on_stack ?scc
    add parameters error string_of_n string_of_e graph bridges
  =
  let error, scc =
    match scc with
    | Some scc -> error, scc
    | None -> Nodearray.create parameters error 1
  in
  let error, pre, low, on_stack, scc_list =
    compute_scc
      ?low ?pre ?on_stack
      parameters error string_of_n graph
  in
  let error, _,scc =
    List.fold_left
      (fun (error, n, scc) cc ->
         let error, n, scc =
           List.fold_left
             (fun (error, n, scc) node ->
                let error, scc = Nodearray.set parameters error node n scc in
                error, n, scc)
             (error, n, scc)
             cc
         in
         error, n+1, scc)
      (error, 1, scc) scc_list
  in
  let error, bridges =
    detect_bridges parameters error add graph string_of_n string_of_e scc bridges
  in
  let error, scc =
    Nodearray.free_all parameters error scc
  in
  error, low, pre, on_stack, scc, bridges



(*cycle detection, keep only components connex?*)

(* remove sub list that contains only one element*)

let remove_one_element_list l =
   let p l = match l with | [_] -> true | [] | _::_::_ -> false
  in
  List.filter (fun x -> not (p x)) l


(*AJOUTER UNE BOUCLE !!! for each list*)
(*create a array of boolean: when the node is in the list return true,
 false otherwise*)
  let tabb parameter error (sl:node list) (dim:node)=
    let error, array = Fixed_size_array.init parameter error dim (fun parameter error _ -> error, false) in
    let error, array =
      List.fold_left
        (fun (error, array) x ->
           Fixed_size_array.set parameter error x true array
        )
        (error, array) sl
    in
    error, array

  (*keep only the nodes that are in the sub graph*)

let filter_graph parameters error graph tabbool =
  (*for each element of the booelan array*)
  let fonc parameters error graphE key data =
    if data then
      begin
        match
          Fixed_size_array.unsafe_get parameters error key graphE
        with
        | error, None -> error, graphE
        | error, Some nod_ed_Lis  ->
          (*donc la on est à chaque fois sur une liste*)
          (*si n = vrai alors on va garder que les noeuds associés à vrai *)
          let istrue parameters error tabbool nod_ed  =
            Fixed_size_array.unsafe_get parameters error (fst nod_ed) tabbool
            (*utiliser list . filter pour le supprimer de la liste : problem a lenvers : on supprime
              pas de la liste mais on retourne la liste qui correspond a vrai ?
              filter : if x n'est pas ds le tableau  *)
          in
          (*la on est que sur la list ( correspondant à une clef donnée...), donc on va supprimer les elements pour cette liste... *)
          let error, new_nod_ed_Lis =
            List.fold_left
              (fun (error, list) elt ->
                 let error, istrue = istrue parameters error tabbool elt in
                 match
                   istrue
                 with
                 | None ->
                   Exception.warn parameters error __POS__ Exit list
                 | Some true ->
                   error, elt::list
                 | Some false -> error,list
              )
              (error,[])
              (List.rev nod_ed_Lis)
          in
          match new_nod_ed_Lis with
          | [] ->
            begin
              Fixed_size_array.free parameters error key graphE;
              Fixed_size_array.free parameters error key graph.node_labels
            end
          | _::_ as l
            ->
            Fixed_size_array.set parameters error key l graphE
            (* filter p l returns all the elements of the list l that satisfy
               the predicate p. The order of the elements in the input list is preserved.
               so Here we wants to delete the elements that???*)
      end
    else begin
      Fixed_size_array.free parameters error key graphE;
      Fixed_size_array.free parameters error key graph.node_labels
    end

  in Fixed_size_array.fold parameters error
    (fun parameters error key data graphE
      -> fonc parameters error graphE key data )
    tabbool graph.edges



let edgeList_onesuccess parameters error graphEdges key =
  (*element de comparaison*)

  let element,lis= key,[]    (*PROBLEME ?*)

  (*on initalise la boucle*)
  (*first element*)
  in
  (*for each key*)
  (*RECUPERER LA LISTE DE NOEUD???*)
  let rec conslis parameters error graphEdges lis element key =

    let error, edg_lis = Nodearray.unsafe_get parameters error key graphEdges in
    (*match la list avec *)
    match edg_lis with
    | None->
      Exception.warn parameters error __POS__ Exit None
    (*if only one element, add this element to the list and continu with that element as the key*)
    | Some []->
      Exception.warn parameters error __POS__ Exit None
    | Some [x,_] ->
      if x != element then
        conslis parameters error graphEdges (x :: lis) element x
      else
        error, Some (List.rev (x :: lis))

    |Some (_::_::_)->
      error,None


  in
  conslis parameters error graphEdges lis element key


(*tests*)
(* ... EN COURS....*)

let agraph =

  let errors = Exception.empty_error_handler in
  let _, parameters, _ = Get_option.get_option errors in

  let nodelabel,
      listnode,
      listedge
    = (fun (x:node)-> x),
      [ (0:node); (1:node); (2:node); (3:node)],

      [ ((0:node),0,(1:node));
        ((1:node),1,(2:node)); ((2:node),2,(3:node)); ((3:node),3,(0:node))]

  in create
    parameters  errors
    nodelabel
    listnode
    listedge




let f graph = (* tout d'abord compute_scc pour avoir les composant connex et récupérer
                 une liste de liste de noeud du graph qui sont connectés*)

  let errors = Exception.empty_error_handler in
  let _, parameters, _ = Get_option.get_option errors in
  let errors,
      _low, _pre,
      _on_stack,
      scc =
    compute_scc
      parameters
      errors
      (fun (x:node) -> "x")
      graph
  in errors,remove_one_element_list scc



(*rajout du tab de booelen et impression en mm temps ?*)

let _ =
  let errors = Exception.empty_error_handler in
  let _, parameters, _ = Get_option.get_option errors in

  (*on récupère les liste de noeud (cycles) qui ont plus d'un élément *)

  let errors,li = f agraph in

  (*let graphtab parameter errors li agraph =  Nodearray.dimension parameters errors agraph.node_labels *)
  (*on veu récupérer la dimension du graph de départ...how ?*)
  let errors, rdim =
    Fixed_size_array.dimension parameters errors agraph.node_labels (*dimen = int? on veut node? a verif *)

  in
  (*for each list in li ( the list of list of node) the function will *)
  let  ite parameters errors rdim eli=
    (* creation du tab de booleen *)
    let errors, tab_bol = tabb parameters errors eli (node_of_int rdim)

    in
    (*impression du tableau (c'est la clé qui change et pour chaque cle on va récupérer
      lélément *)
    let impr parameters errors key e =
      (*ici on récupère  l'element coorespondant a la clef*)
      let ()= Loggers.fprintf (Remanent_parameters.get_logger parameters)
          "%s%i:" (Remanent_parameters.get_prefix parameters)
          key

      in
      let ()= Loggers.fprintf (Remanent_parameters.get_logger parameters)
          "%B "      e

      in
      errors
    in
    let errors =
      Fixed_size_array.iter parameters errors impr tab_bol
      (*fin de l'impr d'un tableau*)

    in
    (*on passe a la fonction qui va supprimer les arrete pour chaque graphes*)
    (*CHANGER LA FONCTION POUR PRENDRE EN COMPTE LE NODELABEL ET RETOURNER *)
    let errors, filgraph = filter_graph parameters errors agraph tab_bol

    in
    (*impression du graph BUUUG *)
    let lh =
      List.map(fun cc->(
            List.map
              (fun x -> int_of_node x)
              cc))agraph.node_labels

    in  List.iter ( fun l -> List.iter
                      (*(Loggers.fprintf (Remanent_parameters.get_logger parameters)
                        "%s%d:" (Remanent_parameters.get_prefix parameters)  l))*)
                      (Printf.printf " %i ")l)
      lh
      (*fin de l'impression du grahp*)
  in
  (*on saute une ligne entre chaque liste *)
  let () =  Loggers.print_newline (Remanent_parameters.get_logger parameters)


  in
  List.iter (fun eli
              -> ite
                  parameters
                  errors
                  rdim
                  eli
            )
    li



(*  Loggers.fprintf (Remanent_parameters.get_logger parameters)
    "%s%i%B:" (Remanent_parameters.get_prefix parameters) key e
    (*convertir la clé en int?*)
    in
    let () =
    Loggers.print_newline (Remanent_parameters.get_logger parameters)*)

    (*
(*fonction d'impression*)

let _=
  let errors = Exception.empty_error_handler in
  let _, parameters, _ = Get_option.get_option errors in
  let errors, newgraph =
    f
      agraph
  in
  let lh =
    List.map(fun cc->(
          List.map
            (fun x -> int_of_node x)
            cc))newgraph

  in  List.iter ( fun l -> List.iter
                    (*(Loggers.fprintf (Remanent_parameters.get_logger parameters)
                      "%s%d:" (Remanent_parameters.get_prefix parameters)  l))*)
                    (Printf.printf " %i ") l)

    lh;;(*newgraph*)
*)
