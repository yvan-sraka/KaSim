(**
   * mvbdu.ml
   * openkappa
   * Jérôme Feret, projet Abstraction, INRIA Paris-Rocquencourt
   * 
   * Creation: 08/03/2010
   * Last modification: 23/11/2010
   * * 
   * This library provides primitives to deal set of finite maps from integers to integers
   *  
   * Copyright 2010 Institut National de Recherche en Informatique et   
   * en Automatique.  All rights reserved.  This file is distributed     
   * under the terms of the GNU Library General Public License *)

let invalid_arg parameters mh message exn value = 
  Exception.warn parameters mh (Some "List_algebra") message exn (fun () -> value)

let rec build_reversed_sorted_list_aux allocate parameters error handler list already = 
  List.fold_left 
    (fun (error,(handler,already)) (var,asso) -> 
      let error,output = 
        List_core.build_list 
          allocate   
          error 
          handler 
          (List_sig.Cons 
             {
               List_sig.variable = var;
               List_sig.association = asso;
               List_sig.tail = already.List_sig.id
             })
          (List_sig.Cons 
             {
               List_sig.variable = var;
               List_sig.association = asso;
               List_sig.tail = already
             })
      in 
      match output with 
        | Some (key,cell,list,handler) -> error,(handler,list)
        | None -> invalid_arg parameters error (Some "Line 41") Exit (handler,already)
    ) 
    (error, (handler, already))
    list

let build_reversed_sorted_list allocate parameters error handler  list = 
  let error,output = 
    List_core.build_list
      allocate 
      error
      handler
      (List_sig.Empty)
      (List_sig.Empty)
  in 
  match output with 
    | Some (key,cell,empty_list,handler) ->
      build_reversed_sorted_list_aux
        allocate
        parameters
        error
        handler
        list
        empty_list
    | None ->
      invalid_arg parameters error (Some "52") Exit
        (handler,
         {List_sig.id = 0;
          List_sig.value = List_sig.Empty})

let build_sorted_list allocate parameters error handler list = 
  build_reversed_sorted_list allocate parameters error handler (List.rev list) 
    
let build_list allocate parameters error handler list = 
  let sort (i,_) (j,_) = - (compare i j) in  
  build_reversed_sorted_list allocate error parameters handler (List.sort sort list)


 			     
let rec print_cell log prefix cell = 
  match cell with 
    | List_sig.Empty -> 
       let s = "[]" in
       let _ = Printf.fprintf log "%s%s\n" prefix s in 
       ()
    | List_sig.Cons x -> 
      let _ = Printf.fprintf log "%s(site_type:%i = %i)\n"
			     prefix 
			     x.List_sig.variable
			     x.List_sig.association
      in
      let prefix' = prefix^" " in 
      let _ = print_association_list log prefix' x.List_sig.tail in
      () 
and print_association_list log prefix list = 
  let _ = Printf.fprintf log "%sId=%i\n" prefix list.List_sig.id in 
  let _ = print_cell log (prefix^" ") list.List_sig.value in 
  ()

let rec print_cell log prefix cell = 
  match cell with 
    | List_sig.Empty -> 
       let s = "[]" in
       let _ = Printf.fprintf log "%s%s\n" prefix s in 
       ()
    | List_sig.Cons x -> 
      let _ = Printf.fprintf log "%s(site_type:%i)\n"
			     prefix 
			     x.List_sig.variable
      in
      let prefix' = prefix^" " in 
      let _ = print_variables_list log prefix' x.List_sig.tail in
      () 
and print_variables_list log prefix list = 
  let _ = Printf.fprintf log "%sId=%i\n" prefix list.List_sig.id in 
  let _ = print_cell log (prefix^" ") list.List_sig.value in 
  ()


let rec overwrite allocate get set error parameters handler list1 list2 = 
  match get parameters error handler (list1,list2)
  with 
    | error, (handler,Some output) -> error, (handler, Some output)
    | error, (handler,None) -> 
      begin
        let error, (handler,output) = 
          match list1.List_sig.value,list2.List_sig.value with 
            | List_sig.Empty,_ -> error,(handler,list2)
	    | _,List_sig.Empty -> error,(handler,list1)
	    | List_sig.Cons a1,List_sig.Cons a2 -> 
	      let var1 = a1.List_sig.variable in 
	      let var2 = a2.List_sig.variable in 
	      let cmp = compare var1 var2 in 
	      let var,asso,tail1,tail2 =
		if cmp < 0
		then
		  var1,a1.List_sig.association,
		  a1.List_sig.tail,list2
		else if cmp = 0 
		then 
		  var1,a2.List_sig.association,
		  a1.List_sig.tail,a2.List_sig.tail
		else 
		  var2,a2.List_sig.association,
		  list1,a2.List_sig.tail
	      in 
	      let error, (handler,tail) =
		overwrite allocate get set error parameters handler tail1 tail2
	      in 
	      match tail with 
	      | Some tail -> 
		let error,output =  
		  List_core.build_list 
		    allocate   
		    error 
		    handler 
		    (List_sig.Cons 
		       {
			 List_sig.variable = var;
			 List_sig.association = asso;
			 List_sig.tail = tail.List_sig.id
		       })
		    (List_sig.Cons 
		       {
			 List_sig.variable = var;
			 List_sig.association = asso;
			 List_sig.tail = tail
		       })
		in
		begin 
		  match output with 
		  | Some (key,cell,list,handler) -> error,(handler,list)
		  | None -> 
		    invalid_arg 
		      parameters 
		      error 
		      (Some "Line 172") 
		      Exit 
		      (handler,{List_sig.id = 0;
				List_sig.value = List_sig.Empty})
		end
	      | None -> 
		 invalid_arg parameters error (Some "171") Exit
		   (handler,
		    {List_sig.id = 0;
		     List_sig.value = List_sig.Empty})
	in 	    
        let error, handler =
          set 
            parameters
            error
            handler
            (list1,list2)
            output
        in 
        error, (handler, Some output)
      end  
