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


(*copy and print*)
(*let copy_tab parameter error print tab =
  (*print function *)
  let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter)
      "START COPY \n" in

  let _ =
    Fixed_size_array.print parameter error
      print
      tab in
  let error, dim = Fixed_size_array.dimension parameter error tab in
  let error, new_tab = Fixed_size_array.expand_and_copy parameter error tab (dim+1) in
  let _ =
    Fixed_size_array.print parameter error
      print
      new_tab
  in
  error, new_tab
*)
(*copy and don't print*)
  let copy_tab parameter error tab =
    (*print function *)
    (*let _ = Loggers.fprintf (Remanent_parameters.get_logger parameter)
        "START COPY \n" in*)

    let error, dim = Fixed_size_array.dimension parameter error tab in
    let error, new_tab = Fixed_size_array.expand_and_copy parameter error tab (dim+1) in
    error, new_tab


let print_node =
    (fun parameters error n ->
       let () = Loggers.fprintf (Remanent_parameters.get_logger parameters)
           "%i\n" n       in error)
let print_edges =
  (fun parameters error l ->
     let error =
       List.fold_left
       (fun error (n,_) ->
          let () = Loggers.fprintf (Remanent_parameters.get_logger parameters)
              "%i-" n in error)
       error l
     in
     let () = Loggers.print_newline (Remanent_parameters.get_logger parameters)
     in error)

(*copy and print*)
(*let copy parameters error graph =
  let error, node_labels = copy_tab parameters error print_node graph.node_labels in
  let error, edges = copy_tab parameters error print_edges graph.edges in
  error, {node_labels;edges}
*)
(*copy and don't print*)
let copy parameters error graph =
  let error, node_labels = copy_tab parameters error graph.node_labels in
  let error, edges = copy_tab parameters error graph.edges in
  error, {node_labels;edges}


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



(*cycle detection, how to detect that each component connex has at most one cycle*)

(* remove sub list that contains only one element*)

let remove_one_element_list l =
   let p l = match l with | [_] -> true | [] | _::_::_ -> false
  in
  List.filter (fun x -> not (p x)) l


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

(*keep only nodes that are in the sub list return a graph with those nodes *)
let filter_graph parameters error (graph:(node,int) graph) tabbool =
  (*for each element of the boolean array*)
  let error, graph = copy parameters error graph in
  let fonc parameters error graphE labels key data =
    if data then
      begin
        match
          Fixed_size_array.unsafe_get parameters error key graphE
        with
        | error, None -> error, graphE, labels
        | error, Some nod_ed_Lis  ->
          let istrue parameters error tabbool nod_ed  =
            Fixed_size_array.unsafe_get parameters error (fst nod_ed) tabbool
          in
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
              let error, graphE = Fixed_size_array.free parameters error key graphE in

              let error, labels = Fixed_size_array.free parameters error key graph.node_labels
              in error, graphE, labels
            end
          | _::_ as l
            ->
            let error, graphE = Fixed_size_array.set parameters error key l graphE in
            error, graphE, labels
      end
    else begin
      let error, graphE =Fixed_size_array.free parameters error key graphE in
      let error, labels =Fixed_size_array.free parameters error key graph.node_labels
      in error, graphE, labels
    end

  in
  let error, (edges, node_labels) =
    Fixed_size_array.fold parameters error
      (fun parameters error key data (graphE, labels)
        ->
          let error, graphE, labels =
            fonc parameters error graphE labels key data
          in
          error, (graphE, labels))
      tabbool (graph.edges, graph.node_labels)
  in
  error, {edges ; node_labels}

(*in the graph, make sure that each egde has only one successor and if so returns
  the list of edge in the right order
  take in argument the graph filtered and the first key with an non empty element*)

let edgeList_onesuccess parameters error graphEdges key =
  let element,lis= key,[]
  in
  (*for each key*)
  let rec conslis parameters error graphEdges lis element key =
    let error, edg_lis = Fixed_size_array.unsafe_get parameters error key graphEdges in
    match edg_lis with
    | None->
      Exception.warn parameters error __POS__ Exit None
    (*if only one element, add the key to the list and continu with that element as the key*)
    | Some []->
      Exception.warn parameters error __POS__ Exit None
    | Some [x,_] ->
      if x != element then
        conslis parameters error graphEdges (key :: lis) element x
      else
        error, Some(List.rev (key :: lis))

    |Some (_::_::_)->
      error,None
  in
  conslis parameters error graphEdges lis element key


(* for each list (that contain more than one element)
   make sure the list has only one cycle and return the list of edges of this cycle
*)
let for_each_list_find_egdesList parameters error graph rdim li =
  (*for each list in li ( the list of list of node) *)
  let  ite parameters error firstgraph rdim eli=
    (* creation of the booelean array *)
    let error, tab_bol = tabb parameters error eli (node_of_int rdim)
    in
    (*filter_graph:  delete egdes that don't appear in the cycle*)
    let error, filgraph = filter_graph parameters error
        firstgraph
        tab_bol
    in
    (* find the first key for function edgeList_onesuccess*)
    let error, keylis = Fixed_size_array.key_list parameters error filgraph.node_labels
    in
    let rec find_first_entry get_entry list error =
      match list with
      | [] -> error, None
      | h::t ->
        begin
          match get_entry parameters error h with
          | error, None -> find_first_entry get_entry t error
          | error, Some _ -> error, Some h
        end
    in
    let find_first_entry_in_fixed_size_array parameters error keylist array =
      find_first_entry
        (fun parameters error key ->
           Fixed_size_array.unsafe_get parameters error key array)
        keylist
        error
    in
    let first_opt =
      find_first_entry_in_fixed_size_array parameters error keylis filgraph.node_labels
    in
    let error,lis =
      match first_opt with
      | error, None ->  error,[]
      | error, Some fk  ->
        let error,licycle =
          edgeList_onesuccess parameters error
            filgraph.edges
            fk
        in
        let lis =
          match licycle with
          |  None ->  []
          |  Some lis->
            lis
        in error, lis
    in lis

  in
  (*use list.map to return the list of results *)
  List.map
    (fun eli
      -> ite
          parameters
          error
          graph
          rdim
          eli
    )
    li


(*compute_scc on a graph and remove form the list that contain only one element*)

let compute_scc_and_remove_one_element_list  graph =

  let error = Exception.empty_error_handler in
  let _, parameters, _ = Get_option.get_option error in
  let error,
      _low, _pre,
      _on_stack,
      scc =
    compute_scc
      parameters
      error
      (fun (x:node) -> "x")
      graph
  in error,remove_one_element_list scc

(*
*******************************
mixture to graph
*******************************
*)

let mixture_to_graph parameters error (mix :mixture) =
  (*initiate list for the graph *)
  let nodelabel = (fun (x:node)-> x) in
  let (listnode : node list) = [] in
  let (listedge : (node * int * node) list )= [] in


  let agentmap = cckappa_sig.mixture.bonds in

  (*for each agent in agentmap ... IN PROGRESS ... *)

  (*copy key list into list of node*)

  let parameter, error, listnode = agentmap. .key_list  in

  (*for each site ( key) in agent_type... IN PROGRESS ... *)
  (*for each site2 (current) in agent_type.execpt the one form the previous loop.. IN PROGRESS ... *)
  if key = current (*a modifier en
                     fonction: on parcourt mais doit etre different de celuide bas*)
       (*Question : what number do I put for the  link?*)
       listedge = ((key:node),Cckappa_sig.site_address.site,(Cckappa_sig.site_address.agent_index:node)) :: listedege
  


  in create
    parameters  error
    nodelabel
    listnode
    listedge





(********************************
FUNCTION TEST
********************************)
(* test function : run if local_trace = true *)

let _  =
  if local_trace then
    let error = Exception.empty_error_handler in
    let _, parameters, _ = Get_option.get_option error in

    let agraph =

      let error = Exception.empty_error_handler in
      let _, parameters, _ = Get_option.get_option error in

      let nodelabel,
          listnode,
          listedge
        = (fun (x:node)-> x),
          [ (0:node); (1:node); (2:node); (3:node); (4:node) ;
            (5:node); (6:node); (7:node); (8:node)],

          [ ((0:node),0,(1:node));
            ((1:node),1,(2:node)); ((2:node),2,(3:node)); ((3:node),3,(4:node));
            ((4:node),4,(5:node));
            ((5:node),5,(6:node)); ((6:node),6,(7:node)); ((7:node),7,(4:node));
            ((3:node),8,(8:node)); ((8:node),9,(1:node))]

      in create
        parameters  error
        nodelabel
        listnode
        listedge
    in

    let error,li = compute_scc_and_remove_one_element_list agraph in

    let error, rdim =
      Fixed_size_array.dimension parameters error agraph.node_labels

    in
    let result  = for_each_list_find_egdesList parameters error agraph rdim li
    in
    (*print result *)
    List.iter
      (fun x ->
         let ()= Loggers.print_newline (Remanent_parameters.get_logger parameters) in
         List.iter (fun x -> Loggers.fprintf (Remanent_parameters.get_logger parameters)
                       "%s%i:" (Remanent_parameters.get_prefix parameters)
                       x)x) result
