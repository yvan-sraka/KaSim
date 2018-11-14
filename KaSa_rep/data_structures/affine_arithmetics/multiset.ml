open Fraction
open Intervalles
open Integer
open Occu1
open Mat_inter

exception Exit

module MultiSet  =
    (struct
      type var = Occu1.trans
      type prod =
        Mvbdu_wrapper.IntMvbdu.mvbdu

      let infinity = 5

      let translate x =
        match x with
        | Occu1.Site x -> Ckappa_sig.int_of_site_name x
        | Occu1.Counter x -> Ckappa_sig.int_of_site_name x
        | Occu1.Bool _ | Occu1.Affine_cst -> assert false

      let print parameters x  =
        if x = infinity then "+oo"
        else if x = - infinity then "-oo"
        else
          string_of_int x

      let gen_opt f a b =
        match a with
        | None -> Some b
        | Some a -> Some (f a b)

      let min a b = if a<b then a else b
      let max a b = if a<b then b else a
      let min_opt a b = gen_opt min a b
      let max_opt a b = gen_opt max a b


      let interval_of_pro _parameters error m x =
        let x = translate x in
        let list = Mvbdu_wrapper.IntMvbdu.build_variables_list [x] in
        let m = Mvbdu_wrapper.IntMvbdu.mvbdu_project_keep_only m list in
        let l = Mvbdu_wrapper.IntMvbdu.extensional_of_mvbdu m in
        match
        List.fold_left
          (fun (inf,sup) l ->
             List.fold_left
               (fun (inf,sup) (_,i) ->
                  min_opt inf i,max_opt sup i)
               (inf,sup) l)
          (None, None) l
        with
          None,_ | _,None -> error, None
        | Some a, Some b -> error, Some (a,
                                         b)

      let string_of_pro parameters error m x =
        let inf_sup = interval_of_pro parameters error m x in
        match inf_sup with
        | error, None -> error, "empty"
        | error, Some (inf,sup) ->
        error, "[|"^(print parameters inf)^
               ";"^(print parameters sup)^"|]"

      let interval_of_pro parameters error m x =
        match interval_of_pro parameters error m x with
        | error, None -> error, None
        | error, Some (a,b) ->
          error, Some (
            (if a = - infinity then
              Fraction.Minfinity
            else
              Fraction.Frac (Fraction.of_int a))
          ,
              if a = infinity then
                Fraction.Infinity
              else
                Fraction.Frac (Fraction.of_int b))

      let is_infinite parameters error m x =
        match interval_of_pro parameters error m x with
        | error, None -> assert false
        | error, Some (_,Fraction.Infinity) -> error, true
        | error, Some _ -> error, false

      let abstract_away parameters error m l =
        let l = List.rev_map translate l in
        let hashed_list =
          Mvbdu_wrapper.IntMvbdu.build_variables_list l
        in
        error, Mvbdu_wrapper.IntMvbdu.mvbdu_project_abstract_away m hashed_list

 let push parameters error m x f  =
	  error, m

 let affiche parameters error prod =
   error

 let create _parameters n =
   Mvbdu_wrapper.IntMvbdu.mvbdu_false ()

 let copy parameters error m = error, m

 let merge parameters error a b = error, Some b
 let solve_all parameters error a = error, Some a

 let union parameters error p q =
   error, Mvbdu_wrapper.IntMvbdu.mvbdu_or p q



 let is_vide m v = false

 let is_empty m =
     Mvbdu_wrapper.IntMvbdu.equal m
     (Mvbdu_wrapper.IntMvbdu.mvbdu_false ())

 let equal m n =
   Mvbdu_wrapper.IntMvbdu.equal m n

 let widen parameters error m n =
   let error, output = union parameters error m n in
   error, (output, not (equal m output))

 let union_incr = widen
 let affiche_mat parameters error m = error

 let compt_of_var_list parameters error l =
   let l = List.rev_map (fun x -> translate x,1) (List.rev l) in
   error, Mvbdu_wrapper.IntMvbdu.mvbdu_of_association_list l

 let guard
     parameters error prod l = error, Some prod
 let plonge parameters error prod l = error, prod
 let solve_inf parameters error prod l = error, Some prod

 let list_var parameters m =
   let a = Mvbdu_wrapper.IntMvbdu.variables_list_of_mvbdu m  in
   let a = Mvbdu_wrapper.IntMvbdu.extensional_of_variables_list a in
   List.rev_map
     (fun a -> Occu1.Site (Ckappa_sig.site_name_of_int a))
     (List.rev a)

 let addzero = false
  end:Mat_inter with type var=Occu1.trans)
