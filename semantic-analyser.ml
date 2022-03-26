(* semantic-analyser.ml
 * The semantic analysis phase of the compiler
 *
 * Programmer: Mayer Goldberg, 2021
 *)

#use "tag-parser.ml";;

exception X_not_yet_implemented;;
exception X_this_should_not_happen;;

type var' = 
  | VarFree of string
  | VarParam of string * int
  | VarBound of string * int * int;;

type expr' =
  | ScmConst' of sexpr
  | ScmVar' of var'
  | ScmBox' of var'
  | ScmBoxGet' of var'
  | ScmBoxSet' of var' * expr'
  | ScmIf' of expr' * expr' * expr'
  | ScmSeq' of expr' list
  | ScmSet' of var' * expr'
  | ScmDef' of var' * expr'
  | ScmOr' of expr' list
  | ScmLambdaSimple' of string list * expr'
  | ScmLambdaOpt' of string list * string * expr'
  | ScmApplic' of expr' * (expr' list)
  | ScmApplicTP' of expr' * (expr' list);;


let var_eq v1 v2 =
match v1, v2 with
  | VarFree (name1), VarFree (name2) -> String.equal name1 name2
  | VarBound (name1, major1, minor1), VarBound (name2, major2, minor2) ->
    major1 = major2 && minor1 = minor2 && (String.equal name1 name2)
  | VarParam (name1, index1), VarParam (name2, index2) ->
       index1 = index2 && (String.equal name1 name2)
  | _ -> false

let rec expr'_eq e1 e2 =
  match e1, e2 with
  | ScmConst' (sexpr1), ScmConst' (sexpr2) -> sexpr_eq sexpr1 sexpr2
  | ScmVar' (var1), ScmVar' (var2) -> var_eq var1 var2
  | ScmIf' (test1, dit1, dif1), ScmIf' (test2, dit2, dif2) -> (expr'_eq test1 test2) &&
                                            (expr'_eq dit1 dit2) &&
                                              (expr'_eq dif1 dif2)
  | (ScmSeq' (exprs1), ScmSeq' (exprs2) | ScmOr' (exprs1), ScmOr' (exprs2)) ->
        List.for_all2 expr'_eq exprs1 exprs2
  | (ScmSet' (var1, val1), ScmSet' (var2, val2) | ScmDef' (var1, val1), ScmDef' (var2, val2)) ->
        (var_eq var1 var2) && (expr'_eq val1 val2)
  | ScmLambdaSimple' (vars1, body1), ScmLambdaSimple' (vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmLambdaOpt' (vars1, var1, body1), ScmLambdaOpt' (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) && (expr'_eq body1 body2)
  | ScmApplic' (e1, args1), ScmApplic' (e2, args2) ->
     (expr'_eq e1 e2) && (List.for_all2 expr'_eq args1 args2)
  | ScmApplicTP' (e1, args1), ScmApplicTP' (e2, args2) ->
      (expr'_eq e1 e2) && (List.for_all2 expr'_eq args1 args2)
  | ScmBox' (v1), ScmBox' (v2) -> var_eq v1 v2
  | ScmBoxGet' (v1), ScmBoxGet' (v2) -> var_eq v1 v2
  | ScmBoxSet' (v1, e1), ScmBoxSet' (v2, e2) -> (var_eq v1 v2) && (expr'_eq e1 e2)
  | _ -> false;;


module type SEMANTIC_ANALYSIS = sig
  val annotate_lexical_addresses : expr -> expr'
  val annotate_tail_calls : expr' -> expr'
  val box_set : expr' -> expr'
  val run_semantics : expr -> expr'
end;; (* end of module type SEMANTIC_ANALYSIS *)

module Semantic_Analysis : SEMANTIC_ANALYSIS = struct

  let rec lookup_in_rib name = function
    | [] -> None
    | name' :: rib ->
       if name = name'
       then Some(0)
       else (match (lookup_in_rib name rib) with
             | None -> None
             | Some minor -> Some (minor + 1));;

  let rec lookup_in_env name = function
    | [] -> None
    | rib :: env ->
       (match (lookup_in_rib name rib) with
        | None ->
           (match (lookup_in_env name env) with
            | None -> None
            | Some(major, minor) -> Some(major + 1, minor))
        | Some minor -> Some(0, minor));;

  let tag_lexical_address_for_var name params env = 
    match (lookup_in_rib name params) with
    | None ->
       (match (lookup_in_env name env) with
        | None -> VarFree name
        | Some(major, minor) -> VarBound(name, major, minor))
    | Some minor -> VarParam(name, minor);;

  let rec map3 f lst p1 p2 =
    match lst with
    | [] -> []
    | hd::tl -> (f hd p1 p2)::(map3 f tl p1 p2);;
    
let unannotate_lexical_address = function
| (VarFree name | VarParam (name, _) | VarBound (name, _, _)) -> ScmVar name

let rec unanalyze expr' =
match expr' with
  | ScmConst' s -> ScmConst(s)
  | ScmVar' var -> unannotate_lexical_address var
  | ScmBox' var -> ScmApplic(ScmVar "box", [unannotate_lexical_address var])
  | ScmBoxGet' var -> unannotate_lexical_address var
  | ScmBoxSet' (var, expr') -> ScmSet (unannotate_lexical_address var, unanalyze expr')
  | ScmIf' (test, dit, dif) -> ScmIf (unanalyze test, unanalyze dit, unanalyze dif)
  | ScmSeq' expr's -> ScmSeq (List.map unanalyze expr's)
  | ScmSet' (var, expr') -> ScmSet (unannotate_lexical_address var, unanalyze expr')
  | ScmDef' (var, expr') -> ScmDef (unannotate_lexical_address var, unanalyze expr')
  | ScmOr' expr's -> ScmOr (List.map unanalyze expr's)
  | ScmLambdaSimple' (params, expr') ->
        ScmLambdaSimple (params, unanalyze expr')
  | ScmLambdaOpt'(params, param, expr') ->
        ScmLambdaOpt (params, param, unanalyze expr')
  | (ScmApplic' (expr', expr's) | ScmApplicTP' (expr', expr's)) ->
        ScmApplic (unanalyze expr', List.map unanalyze expr's);;

let string_of_expr' expr' =
    string_of_expr (unanalyze expr');;

  (* run this first! *)
  let annotate_lexical_addresses pe = 
   let rec run pe params env =
      match pe with 
      |ScmConst a -> ScmConst' a
      |ScmVar a -> ScmVar'(tag_lexical_address_for_var a params env)
      |ScmIf(test, _then, _else) -> ScmIf'(run test params env, run _then params env, run _else params env)
      |ScmSeq s -> ScmSeq'(map3 run s params env)
      |ScmSet(ScmVar a, value) -> ScmSet'((tag_lexical_address_for_var a params env), (run value params env))
      |ScmDef(ScmVar a, value) -> ScmDef'(VarFree a, (run value params env))
      |ScmOr list -> ScmOr'(map3 run list params env)
      |ScmLambdaSimple(args, body) -> ScmLambdaSimple'(args, run body args ([params]@env))
      |ScmLambdaOpt(args, restArgs, body) -> ScmLambdaOpt'(args, restArgs, run body (args@[restArgs]) ([params]@env))
      |ScmApplic(op, seq) -> ScmApplic'(run op params env, map3 run seq params env)
      | _ -> raise X_this_should_not_happen 
   in 
   run pe [] [];;

  let rec rdc_rac s =
    match s with
    | [e] -> ([], e)
    | e :: s ->
       let (rdc, rac) = rdc_rac s
       in (e :: rdc, rac)
    | _ -> raise X_this_should_not_happen;; 
  
  (* run this second! *)
  let annotate_tail_calls pe =
   let rec run pe in_tail =
      match pe with
      |ScmConst' (_)  | ScmVar' (_)-> pe 
      |ScmIf'(test, _then, _else) -> ScmIf'(run test false, run _then in_tail, run _else in_tail) (**Should I check _then+_else return true? *)
      |ScmSeq' seq -> 
        let (firsts, last) = rdc_rac seq in 
        ScmSeq'( (List.map (fun pe -> (run pe false)) firsts)@[(run last in_tail)]  )
      |ScmOr' lst -> 
        let (firsts, last) = rdc_rac lst in 
        ScmOr'( (List.map (fun pe -> (run pe false)) firsts)@[(run last in_tail)]  )
      |ScmSet'(var, value) -> ScmSet'(var, (run value false))
      |ScmDef'(var, value) -> ScmDef'(var, (run value false))
      |ScmLambdaSimple'(args, body) -> ScmLambdaSimple'(args, (run body true))
      |ScmLambdaOpt'(args, restArgs, body) -> ScmLambdaOpt'(args, restArgs, (run body true))
      |ScmApplic'(op, seq) -> 
        match in_tail with 
        |true -> ScmApplicTP'( (run op false), (List.map (fun pe -> (run pe false)) seq) )
        |false -> ScmApplic'( (run op false), (List.map (fun pe -> (run pe false)) seq) )
      |_ -> raise X_not_yet_implemented 
   in 
   run pe false;;

  (* boxing *)

  let rec box_set expr = 
    match expr with 
    |ScmBoxSet'(var, value) -> ScmBoxSet' (var, box_set value)
    |ScmIf'(test, _then, _else)-> ScmIf'(box_set test, box_set _then, box_set _else) 
    |ScmSeq' seq -> ScmSeq' (List.map (fun exp -> box_set exp) seq)
    |ScmSet'(var, value) -> ScmSet' (var, box_set value)
    |ScmDef' (var, value) -> ScmDef' (var, box_set value)
    |ScmOr' lst -> ScmOr' (List.map (fun exp -> box_set exp) lst)
    |ScmLambdaSimple' (args, body)  -> ScmLambdaSimple'(args, rmvSeq (box_set (box_args_helper args body [])))
    |ScmLambdaOpt' (args, restArgs, body) -> ScmLambdaOpt'(args, restArgs, rmvSeq (box_set (box_args_helper (args@[restArgs]) body [])))
    |ScmApplic' (op, seq) -> ScmApplic' (box_set op, List.map (fun rib -> box_set rib) seq)
    |ScmApplicTP' (op, seq) -> ScmApplicTP' (box_set op, List.map (fun rib -> box_set rib) seq)
    | _ -> expr
    
    and box_args_helper args expr acc = 
      match args with
      |[] -> (box_all expr acc)
      |hd::tl -> 
        let should_box = should_be_boxed hd expr in
        let new_acc = if (should_box) then (acc@[hd]) else acc in
        (box_args_helper tl expr new_acc)

    and box_all expr args_to_box = 
      let new_expr = replace_get_set_loop args_to_box expr in
      let boxed = List.mapi (fun i arg -> make_box arg i) args_to_box in
      ScmSeq' (boxed@[(box_set new_expr)])

    and replace_get_set_loop args_to_box expr = 
      match args_to_box with 
      |[] -> expr
      |hd::tl -> 
        let new_expr = replace_get_set hd expr in
        (replace_get_set_loop tl new_expr)

    and replace_get_set param expr = 
      match expr with 
      | ScmSet' (VarBound (arg, mj, mi), exp) -> if (arg = param) then ScmBoxSet' (VarBound (arg, mj, mi), (replace_get_set param exp)) else ScmSet' (VarBound (arg, mj, mi), (replace_get_set param exp))
      | ScmSet' (VarParam (arg, mi), exp) ->  if arg = param then ScmBoxSet' (VarParam (arg, mi), (replace_get_set param exp)) else ScmSet' (VarParam (arg, mi), (replace_get_set param exp))
      | ScmVar' (VarBound (arg, mj, mi)) -> if arg = param then ScmBoxGet' (VarBound (arg, mj, mi)) else expr
      | ScmVar' (VarParam (arg, mi)) -> if arg = param then ScmBoxGet' (VarParam (arg, mi)) else expr
      | ScmBoxSet' (v,exp) -> ScmBoxSet' (v ,(replace_get_set param exp)) 
      | ScmIf'(test, _then, _else)-> ScmIf' ((replace_get_set param test), (replace_get_set param _then), (replace_get_set param _else))
      | ScmSeq' seqLst -> ScmSeq' (List.map (fun exp -> (replace_get_set param exp)) seqLst)
      | ScmDef' (v, exp) -> ScmDef' (v, (replace_get_set param exp))
      | ScmOr' expLst -> ScmOr' (List.map (fun exp -> replace_get_set param exp) expLst)
      | ScmLambdaSimple' (args, exps)  -> if List.mem param args then ScmLambdaSimple' (args, exps) else ScmLambdaSimple' (args, replace_get_set param exps)
      | ScmLambdaOpt' (args, restArgs,exps) -> if List.mem param args || List.mem param [restArgs] then ScmLambdaOpt' (args, restArgs, exps) else ScmLambdaOpt' (args, restArgs, replace_get_set param exps)
      | ScmApplic' (proc, args) -> ScmApplic' ((replace_get_set param proc) , List.map (fun arg -> (replace_get_set param arg)) args)
      | ScmApplicTP' (proc, args) -> ScmApplicTP' ((replace_get_set param proc) , List.map (fun arg -> (replace_get_set param arg)) args)
      | _ -> expr
    
    and rmvSeq exp = 
      match exp with
      | ScmSeq' [e] -> e 
      |  _ -> exp 

    and should_be_boxed arg expr = 
      let (get, depth_get, set, depth_set, curr_depth) = check_conditions arg expr (false, 0, false, 0, 0) in
      get && set && (depth_get <> depth_set)

    (**Notice that when arg is used as VarBound it always involves 'get' or 'set' *)
    and check_conditions arg expr (get, depth_get, set, depth_set, curr_depth) = 
      if get && set && (depth_get <> depth_set)
      then (get, depth_get, set, depth_set, curr_depth)
      else 
      (
        match expr with 
        |ScmBoxSet'(_, e) -> check_conditions arg e (get, depth_get, set, depth_set, curr_depth)
        |ScmSet'(VarBound(name, _, _), value) when name=arg -> check_conditions arg value (get, depth_get, true, curr_depth, curr_depth)
        |ScmSet'(VarBound(name, _, _), value) when name<>arg -> check_conditions arg value (get, depth_get, set, depth_set, curr_depth)

        |ScmSet'(VarParam(name,_), value) when name=arg -> check_conditions arg value (get, depth_get, true, curr_depth, curr_depth)
        |ScmSet'(VarParam(name,_), value) when name<>arg -> check_conditions arg value (get, depth_get, set, depth_set, curr_depth)

        |ScmVar' (VarBound(name,_,_)) when name=arg -> (true, curr_depth, set, depth_set, curr_depth)
        |ScmVar' (VarBound(name,_,_)) when name<>arg -> (get, depth_get, set, depth_set, curr_depth)
      
        |ScmVar' (VarParam(name,_)) when name=arg -> (true, curr_depth, set, depth_set, curr_depth)
        |ScmVar' (VarParam(name,_)) when name<>arg -> (get, depth_get, set, depth_set, curr_depth)

        (**We are checking for usage in different ribs *)
        |ScmIf'(test, _then, _else) -> loop arg (test::_then::_else::[]) (get, depth_get, set, depth_set, curr_depth)

        |ScmSeq' seq | ScmOr' seq -> loop arg seq (get, depth_get, set, depth_set, curr_depth)
        |ScmDef'(_, e) | ScmSet'(_, e) -> check_conditions arg e (get, depth_get, set, depth_set, curr_depth)
        |ScmLambdaSimple'(args, body) when (List.mem arg args)-> (false, depth_get, false, depth_set, curr_depth)
        |ScmLambdaSimple'(args, body) when not (List.mem arg args) -> check_conditions arg body (get, depth_get, set, depth_set, curr_depth + 1)
        |ScmLambdaOpt'(args, restArgs, body) when (List.mem arg (args@[restArgs])) -> (false, depth_get, false, depth_set, curr_depth)
        |ScmLambdaOpt'(args, restArgs, body) when not (List.mem arg (args@[restArgs])) -> check_conditions arg body (get, depth_get, set, depth_set, curr_depth + 1)
        |ScmApplic'(op, seq) | ScmApplicTP'(op, seq) -> loop arg (op::seq) (get, depth_get, set, depth_set, curr_depth)
        |_ -> (get, depth_get, set, depth_set, curr_depth)

      ) 


    and loop arg lst (get, depth_get, set, depth_set, curr_depth) = 
      match lst with
      |[] -> (get, depth_get, set, depth_set, curr_depth)
      |hd::tl ->
        let (get', depth_get', set', depth_set', curr_depth') = check_conditions arg hd (get, depth_get, set, depth_set, curr_depth) in
        if get' && set' && (depth_get' <> depth_set')
        then (get', depth_get', set', depth_set', curr_depth')
        else loop arg tl (get', depth_get', set', depth_set', curr_depth)
        
        
    and make_box param index = ScmSet'(VarParam(param, index), ScmBox'(VarParam(param, index)))
    
    ;;

  let run_semantics expr =
    box_set
      (annotate_tail_calls
         (annotate_lexical_addresses expr))

end;; (* end of module Semantic_Analysis *)
