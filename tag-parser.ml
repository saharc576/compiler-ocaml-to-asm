#use "reader.ml";;

type expr =
  | ScmConst of sexpr
  | ScmVar of string
  | ScmIf of expr * expr * expr
  | ScmSeq of expr list
  | ScmSet of expr * expr
  | ScmDef of expr * expr
  | ScmOr of expr list
  | ScmLambdaSimple of string list * expr
  | ScmLambdaOpt of string list * string * expr
  | ScmApplic of expr * (expr list);;

exception X_syntax_error of sexpr * string;;
exception X_reserved_word of string;;
exception X_proper_list_error;;
exception X_not_implemented;;
exception X_this_should_not_happen;;

let rec list_to_proper_list = function
| [] -> ScmNil
| hd::[] -> ScmPair (hd, ScmNil)
| hd::tl -> ScmPair (hd, list_to_proper_list tl)

let rec list_to_improper_list = function
| [] -> raise X_proper_list_error
| hd::[] -> hd
| hd::tl -> ScmPair (hd, list_to_improper_list tl)

let rec scm_append scm_list sexpr =
match scm_list with
| ScmNil -> sexpr
| ScmPair (car, cdr) -> ScmPair (car, scm_append cdr sexpr)
| _ -> raise (X_syntax_error (scm_list, "Append expects a proper list"))

let rec scm_map f sexpr =
match sexpr with
| ScmNil -> ScmNil
| ScmPair (car, cdr) -> ScmPair (f car, scm_map f cdr)
| _ -> raise (X_syntax_error (sexpr, "Map expects a list"));;

let rec scm_zip f sexpr1 sexpr2 =
match sexpr1, sexpr2 with
| ScmNil, ScmNil -> ScmNil
| ScmPair (car1, cdr1), ScmPair (car2, cdr2) -> ScmPair (f car1 car2, scm_zip f cdr1 cdr2)
| _, _ ->
    let sepxrs = list_to_proper_list [ScmSymbol "sexpr1:"; sexpr1; ScmSymbol "sexpr2:"; sexpr2] in
    raise (X_syntax_error (sepxrs, "Zip expects 2 lists of equal length"));;

let rec scm_list_to_list = function
| ScmPair (hd, tl) -> hd::(scm_list_to_list tl)
| ScmNil -> []
| sexpr -> raise (X_syntax_error (sexpr, "Expected proper list"));;

let rec scm_is_list = function
| ScmPair (hd, tl) -> scm_is_list tl
| ScmNil -> true
| _ -> false

let rec scm_list_length = function
| ScmPair (hd, tl) -> 1 + (scm_list_length tl)
| ScmNil -> 0
| sexpr -> raise (X_syntax_error (sexpr, "Expected proper list"));;

let rec untag expr =
let untag_vars vars = List.map (fun s -> ScmSymbol s) vars in
let untag_tagged_list tag exprs = list_to_proper_list (ScmSymbol tag::(List.map untag exprs)) in

let untag_lambda_opt vars var body =
let vars = match vars with
| [] -> ScmSymbol var
| _ -> list_to_improper_list (untag_vars (vars@[var])) in
list_to_proper_list ([ScmSymbol "lambda"; vars]@body) in

match expr with
| (ScmConst (ScmSymbol(_) as sexpr)
    | ScmConst (ScmNil as sexpr)
    | ScmConst (ScmPair (_, _) as sexpr)) -> list_to_proper_list [ScmSymbol "quote"; sexpr]
| ScmConst s -> s
| ScmVar (name) -> ScmSymbol(name)
| ScmIf (test, dit, ScmConst (ScmVoid)) -> untag_tagged_list "if" [test; dit]
| ScmIf (test, dit, dif) -> untag_tagged_list "if" [test; dit; dif]
| ScmSeq(exprs) -> untag_tagged_list "begin" exprs
| ScmSet (var, value) -> untag_tagged_list "set!" [var; value]
| ScmDef (var, value) -> untag_tagged_list "define" [var; value]
| ScmOr (exprs) -> untag_tagged_list "or" exprs
| ScmLambdaSimple (vars, ScmSeq(body)) ->
    let vars = list_to_proper_list (untag_vars vars) in
    let body = List.map untag body in
    list_to_proper_list ([ScmSymbol "lambda"; vars]@body)
| ScmLambdaSimple (vars, body) ->
    let vars = list_to_proper_list (untag_vars vars) in
    list_to_proper_list ([ScmSymbol "lambda"; vars; untag body])
| ScmLambdaOpt (vars, var, ScmSeq(body)) ->
    let body = List.map untag body in
    untag_lambda_opt vars var body
| ScmLambdaOpt (vars, var, body) ->
    let body = [untag body] in
    untag_lambda_opt vars var body
| ScmApplic(procedure, args) -> list_to_proper_list (List.map untag (procedure::args));;


let rec string_of_expr expr =
string_of_sexpr (untag expr)

let scm_number_eq n1 n2 =
match n1, n2 with
| ScmRational(numerator1, denominator1), ScmRational(numerator2, denominator2) ->
        numerator1 = numerator2 && denominator1 = denominator2
| ScmReal(real1), ScmReal(real2) -> abs_float(real1 -. real2) < 0.001
| _, _ -> false

let rec sexpr_eq s1 s2 =
match s1, s2 with
| (ScmVoid, ScmVoid) | (ScmNil, ScmNil)  -> true
| ScmBoolean(bool1), ScmBoolean(bool2) -> bool1 = bool2
| ScmChar(char1), ScmChar(char2) -> char1 = char2
| ScmString(string1), ScmString(string2) -> String.equal string1 string2
| ScmSymbol(symbol1), ScmSymbol(symbol2) -> String.equal symbol1 symbol2
| ScmNumber(number1), ScmNumber(number2) -> scm_number_eq number1 number2
| ScmVector(sexprs1), ScmVector(sexprs2) -> List.for_all2 sexpr_eq sexprs1 sexprs2
| ScmPair(car1, cdr1), ScmPair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
| _, _ -> false

let rec expr_eq e1 e2 =
  match e1, e2 with
  | ScmConst (sexpr1), ScmConst (sexpr2) -> sexpr_eq sexpr1 sexpr2
  | ScmVar (var1), ScmVar (var2) -> String.equal var1 var2
  | ScmIf (test1, dit1, dif1), ScmIf (test2, dit2, dif2) -> (expr_eq test1 test2) &&
                                            (expr_eq dit1 dit2) &&
                                              (expr_eq dif1 dif2)
  | (ScmSeq(exprs1), ScmSeq(exprs2) | ScmOr (exprs1), ScmOr (exprs2)) ->
        List.for_all2 expr_eq exprs1 exprs2
  | (ScmSet (var1, val1), ScmSet (var2, val2) | ScmDef (var1, val1), ScmDef (var2, val2)) ->
        (expr_eq var1 var2) && (expr_eq val1 val2)
  | ScmLambdaSimple (vars1, body1), ScmLambdaSimple (vars2, body2) ->
     (List.for_all2 String.equal vars1 vars2) && (expr_eq body1 body2)
  | ScmLambdaOpt (vars1, var1, body1), ScmLambdaOpt (vars2, var2, body2) ->
     (String.equal var1 var2) &&
       (List.for_all2 String.equal vars1 vars2) && (expr_eq body1 body2)
  | ScmApplic (e1, args1), ScmApplic (e2, args2) ->
     (expr_eq e1 e2) && (List.for_all2 expr_eq args1 args2)
  | _ -> false;;

module type TAG_PARSER = sig
  val tag_parse_expression : sexpr -> expr
end;; 

module Tag_Parser : TAG_PARSER = struct

let reserved_word_list =
  ["and"; "begin"; "cond"; "define"; "else";
   "if"; "lambda"; "let"; "let*"; "letrec"; "or";
   "quasiquote"; "quote"; "set!"; "unquote";
   "unquote-splicing"];;

let rec tag_parse_expression sexpr =
let sexpr = macro_expand sexpr in
match sexpr with 
    (*Unquoted = Self evaluating *)
    | ScmNil | ScmBoolean(_) | ScmChar(_) | ScmNumber(_) | ScmString(_) 
    -> ScmConst(sexpr) 
    (*Quoted = Not self evaluating *)
    | ScmPair(ScmSymbol("quote"), ScmPair(e, ScmNil)) -> ScmConst(e) (*Remove the first and create const *)

    (*Variables *)
    | ScmSymbol(x) -> if (is_reserved x) then raise (X_reserved_word(x)) else ScmVar(x)

    (*Conditionals = if <test> <then> <else> *) 
    | ScmPair(ScmSymbol("if"), rest) -> parse_if_exp rest             

    (*Lambda*)
    |ScmPair(ScmSymbol("lambda"), rest) -> parse_lambda_exp rest

    (*Disjunctions*)
    | ScmPair(ScmSymbol("or"), rest) -> parse_or_exp rest

    (*Define  = define <var> <val> *)
    | ScmPair(ScmSymbol("define"), rest) -> parse_define_exp rest
    (*Set! <var> <val> *)
    | ScmPair(ScmSymbol("set!"), rest) -> parse_set_exp rest

    (*Sequences = begin <e1>...<em> *)
    | ScmPair(ScmSymbol("begin"), rest) -> parse_seq_exp rest
    
    (*Applications *)
    | ScmPair(procedure, args) -> parse_applic_exp sexpr
    
    | _ -> raise (X_syntax_error (sexpr, "Sexpr structure not recognized"))

and macro_expand sexpr =
match sexpr with
        (*And *)
        | ScmPair(ScmSymbol("and"),rest) -> parse_and_exp rest

        (*Cond *)
        | ScmPair(ScmSymbol("cond"), ribs) -> expend_cond ribs

        (*Let star *)
        | ScmPair(ScmSymbol("let*"), rest ) -> expend_let_star rest

        (*Let *)
        | ScmPair(ScmSymbol("let"), rest) -> expend_let rest

        (*Letrec *)
        | ScmPair(ScmSymbol "letrec", rest) -> expand_letrec rest

        (*Quasiquote = quasiquote <sexpr> *)
        | ScmPair(ScmSymbol("quasiquote"), ScmPair(rest, ScmNil )) -> expend_quasiquote rest

        | _ -> sexpr

(*---- Auxilairy parsers ------------------------------------------------------------------------------- *)

and is_reserved w = 
    if (List.mem w reserved_word_list) then true else false

and proper_list_to_string list =
  match list with
  | ScmNil -> []
  | ScmPair(ScmSymbol(a),b) -> ((a)::(proper_list_to_string b))
  | _ -> raise (X_syntax_error(list, "proper_list_to_string->Expression is not a valid Lambda expression"))

and improper_list_to_string list =
  match list with
  | ScmSymbol(x) -> []
  | ScmPair(ScmSymbol(a),ScmSymbol(b)) -> [a]
  | ScmPair(ScmSymbol(a),b) -> ((a)::(improper_list_to_string b))
  | _ -> raise (X_syntax_error(list, "improper_list_to_string->Expression is not a valid Lambda expression"))

and last_item list =
  match list with
  | ScmSymbol(a) -> a
  | ScmPair(ScmSymbol(a),ScmSymbol(b)) -> b
  | ScmPair(ScmSymbol(a),b) -> (last_item b)
  | _ -> raise (X_syntax_error(list, "last_item->Expression is not a valid Lambda expression"))

    and parse_if_exp exp = 
        match exp with
        (*Without else clause *)
        | ScmPair(test, ScmPair(_then, ScmNil))
        ->
        let parsed_test = tag_parse_expression test in
        let parsed_then = tag_parse_expression _then in
        ScmIf(parsed_test, parsed_then, ScmConst(ScmVoid))
        
        (*With else clause *)
        | ScmPair(test, ScmPair(_then, ScmPair(_else, ScmNil))) 
        -> 
        let parsed_test = tag_parse_expression test in
        let parsed_then = tag_parse_expression _then in
        let parsed_else = tag_parse_expression _else in
        ScmIf(parsed_test, parsed_then, parsed_else)
        
        (*Any other case - error *)
        | _ -> raise (X_syntax_error(exp, "Expression is not a valid If expression")) 
  
    and parse_or_exp exp =
        match exp with
        (*Disjunction of zero expressions is false*)
        | ScmNil -> ScmConst(ScmBoolean false)

        | ScmPair(a,b) -> 
                        let result = List.map tag_parse_expression (scm_list_to_list exp) in
                        match List.length result with 
                        | 1 -> List.hd result
                        | _ -> ScmOr(result) 

        (*Any other case - error *)
        | _ -> raise (X_syntax_error( exp, "Invalid disjunction"))

    and parse_define_exp exp = 
        match exp with 
        (*Symple define *)
        | ScmPair(ScmSymbol(v), ScmPair(body, ScmNil))
        ->
        if ((is_reserved v) = true)
        then raise (X_syntax_error(exp, "Expected variable on LHS of define")) 
        else ScmDef(ScmVar(v), tag_parse_expression body)  
        
        (*MIT define *)
        | ScmPair(args, body) 
        -> (
            match args with
            | ScmPair(symb, rest_args) -> 
                let _lambda_simp_ = parse_lambda_exp (ScmPair(rest_args, body)) in
                ScmDef(tag_parse_expression symb, _lambda_simp_)
            ) 
        (*Any other case - error *)
        | _ -> raise (X_syntax_error( exp, "Invalid define"))

    and parse_and_exp exp = 
        match exp with
        |ScmNil -> macro_expand(ScmBoolean true)
        |ScmPair(x,ScmNil)-> macro_expand x
        |ScmPair(a,b) -> (macro_expand(ScmPair (ScmSymbol "if",ScmPair (a, ScmPair (ScmPair(ScmSymbol("and"),b),ScmPair(ScmBoolean false, ScmNil))))))
        (*Any other case - error *)
        | _ -> raise (X_syntax_error( exp, "Expression is not a valid And expression")) 

    and parse_set_exp exp = 
        match exp with 
        | ScmPair(ScmSymbol(v), ScmPair(value_exp, ScmNil))
        ->
        if ((is_reserved v) = true)
        then raise (X_syntax_error(exp, "Expected variable on LHS of set!")) 
        else ScmSet(ScmVar(v), tag_parse_expression value_exp)
        (*Any other case - error *)
        | _ -> raise (X_syntax_error( exp, "Expected variable on LHS of set!"))

    and pairs_to_list list =
        match list with
        | ScmNil -> []
        | ScmPair (x, ScmNil) -> ((tag_parse_expression x)::[])
        | ScmPair(a,b) -> ((tag_parse_expression a)::(pairs_to_list b))
    
    and parse_list_for_applic scm_list = 
        let _list_ = (scm_list_to_list scm_list) in
        let _list_ = (List.map (fun a -> (match a with
                                            | ScmPair(x, rest) -> 
                                                if (sexpr_eq x (ScmSymbol "quote") ) then ScmPair(x, rest) else x 
                                            | _ -> a))  _list_) in
        let _parsed_list_ = (List.map tag_parse_expression _list_) in
        _parsed_list_

     and parse_applic_exp exp=
         match exp with
            | ScmPair (procedure,args) -> 
                (
                    let parsed_args = List.map tag_parse_expression (scm_list_to_list args) in
                    match procedure with
                    | ScmSymbol "begin" -> 
                        (
                            match parsed_args with
                            | [e] -> e
                            | _ -> ScmSeq parsed_args
                        )
                    | _ -> (**Not begin *)
                        (
                            match procedure with
                            | ScmSymbol sym ->
                                if (is_reserved sym) 
                                then ScmApplic(tag_parse_expression procedure, parsed_args)
                                else ScmApplic(ScmVar(sym), parsed_args)
                            | _ -> ScmApplic(tag_parse_expression procedure, parsed_args)
                        )
                )
            | _ -> raise (X_syntax_error( exp, "Expression is not a valid Application expression")) 

    and parse_seq_exp exp = 
        let _list_ = (scm_list_to_list exp) in
        let _parsed_list_ = (List.map tag_parse_expression _list_) in
        match _parsed_list_ with 
        | [e] -> e
        | _ -> ScmSeq(_parsed_list_)

    and check_args_type args =
        match args with
        | ScmNil -> "simple"
        | ScmSymbol(b) -> "opt"
        | ScmPair(ScmSymbol(a),b) -> (check_args_type b)
        (*Any other case - error *)
        | _ -> raise (X_syntax_error(args, "check_args_type->Expression is not a valid Lambda expression"))

    and parse_lambda_exp rest = 
        match rest with
        | ScmNil -> raise (X_syntax_error(rest, "parse_lambda_exp->Expression is not a valid Lambda expression"))
        | ScmPair (exp, ScmNil) -> raise (X_syntax_error(rest, "parse_lambda_exp->Expression is not a valid Lambda expression"))
        |ScmPair (args,body) ->
                let args_type = (check_args_type args) in
                match args_type with
                | "simple" -> ScmLambdaSimple(proper_list_to_string args, parse_seq_exp body)
                | "opt" -> ScmLambdaOpt(improper_list_to_string args, last_item args, parse_seq_exp body)
                (*Any other case - error *)
                | _ -> raise (X_syntax_error(args, "parse_lambda_exp->Expression is not a valid Lambda expression"))

(*---- End Auxilairy parsers ------------------------------------------------------------------------------- *)

(*---- Auxilairy macro-expenders ------------------------------------------------------------------------------- *)
     and expend_quasiquote exp = 
     match exp with
     | ScmNil | ScmSymbol(_) -> ScmPair (ScmSymbol("quote"), ScmPair (exp, ScmNil))
     | ScmVector(list) -> ScmPair(ScmSymbol "list->vector",ScmPair(expend_quasiquote (list_to_proper_list list),ScmNil))
     | ScmPair(ScmSymbol("unquote"), ScmPair (rest, ScmNil)) -> rest
     | ScmPair(ScmSymbol("unquote-splicing"), rest) -> ScmPair (ScmSymbol("quote"), ScmPair (exp, ScmNil))  
     | ScmPair (x,y) -> qq_pair x y
     | _ -> exp

     and qq_pair x y=
     match x,y with
     | ScmPair(ScmSymbol("unquote-splicing"), ScmPair (sexpression, ScmNil)), _ -> ScmPair(ScmSymbol("append"), ScmPair(sexpression, ScmPair((expend_quasiquote y), ScmNil ))) 
     | _ , _ ->  ScmPair(ScmSymbol "cons", ScmPair((expend_quasiquote x), ScmPair((expend_quasiquote y), ScmNil))) 

     and expend_cond ribs = 
        let rec parse_ribs ribs = 
            match ribs with
            
            (*Else rib - ignore all the rest of the ribs*)
            | ScmPair (ScmPair (ScmSymbol("else"), sequence), rest_ignore) -> ScmPair(ScmSymbol("begin"),sequence) 

           (*Arrow rib *)
            | ScmPair (ScmPair(test_, ScmPair (ScmSymbol("=>"), sequence)), rest_ribs)  
             ->
             let _test_vars_ = ScmPair(ScmSymbol("value"), ScmPair(test_, ScmNil)) in
             let _other_vars = ScmPair(ScmSymbol("f"), ScmPair(ScmPair(ScmSymbol("lambda"), ScmPair(ScmNil, sequence)),ScmNil )) in
             (* let _other_vars = ScmPair(_other_vars, ScmNil) in *)
             let _rest_ = ScmPair(ScmSymbol("rest"),ScmPair( ScmPair(ScmSymbol("lambda"), ScmPair(ScmNil, ScmPair(parse_ribs rest_ribs, ScmNil))), ScmNil)) in

            if (sexpr_eq rest_ribs ScmNil)
            then 
                let _f_ = ScmPair (ScmPair (ScmSymbol "f", ScmNil), ScmPair (ScmSymbol "value", ScmNil)) in
                let _if_ = ScmPair (ScmSymbol "if",ScmPair(ScmSymbol ("value"), ScmPair(_f_, ScmNil))) in
                let _let_ = ScmPair(ScmSymbol("let"), ScmPair(ScmPair(_test_vars_, _other_vars),_if_)) in
                _let_

            else 
                let _f_ = ScmPair (ScmPair (ScmSymbol "f", ScmNil), ScmPair (ScmSymbol "value", ScmNil)) in
                let _if_ = ScmPair (ScmSymbol "if",ScmPair(ScmSymbol ("value"), ScmPair(_f_, ScmPair (ScmPair (ScmSymbol "rest", ScmNil), ScmNil)))) in
                let _let_ = ScmPair(ScmSymbol("let"), ScmPair(ScmPair(_test_vars_, ScmPair(_other_vars, ScmPair(_rest_, ScmNil))),ScmPair(_if_, ScmNil))) in
                _let_
                
            (*Simple cond-ribs *) 
            | ScmPair ((ScmPair(test_, sequence)), rest_of_ribs)
             -> 
             let _test_ = test_ in
             let _then_ = ScmPair(ScmSymbol("begin"), sequence) in
             let _else_ = parse_ribs rest_of_ribs in
             if (sexpr_eq _else_ ScmNil)
             then ScmPair(ScmSymbol("if"), ScmPair(_test_, ScmPair(_then_, _else_)))
             else ScmPair(ScmSymbol("if"), ScmPair(_test_, ScmPair(_then_, ScmPair(_else_, ScmNil))))

            | ScmNil -> ScmNil

            | _ -> raise X_not_implemented
             in
        (macro_expand(parse_ribs ribs))

    and expend_let_star sexp = 
        let rec parse_let_star args body = 
            match args with
            
            (*first base case- No args *)
            | ScmNil ->
                ScmPair(  ScmSymbol("let"), ScmPair(args, body)  )
            
            (*Second base case - One argument *)
            | ScmPair(curr_arg, ScmNil) ->
                ScmPair(  ScmSymbol("let"), ScmPair(args, body)  )
            
            (*Third base case - More than one argument *)
            | ScmPair(curr, rest_args) ->
                ScmPair(  ScmSymbol("let"), ScmPair(  ScmPair(curr, ScmNil), ScmPair(  parse_let_star rest_args body  , ScmNil)  )    )     

            | _ -> raise (X_syntax_error(sexp, "Not a valid let* Sexpression"))
            in

        match sexp with 
        | ScmPair(args, body) -> (macro_expand (parse_let_star args body))
        | _ -> raise (X_syntax_error(sexp, "Let* should have args and body") )

    and get_arg_name = function
        |ScmNil -> ScmNil
        |ScmPair(sym, _) -> sym
        |_ -> raise X_this_should_not_happen
    
    and get_value = function
        |ScmNil -> ScmNil
        |ScmPair(_, ScmPair(value, ScmNil)) -> value
        |_ -> raise X_this_should_not_happen

    and expend_let sexp = 
        match sexp with 
            | ScmPair(ScmNil, body)-> ScmPair(ScmPair(ScmSymbol "lambda",ScmPair(ScmNil, body)),ScmNil)
            | ScmPair(args, body) -> 
                let _parsed_args = scm_map get_arg_name args in
                let _parsed_vals = scm_map get_value args in
                ScmPair((ScmPair(ScmSymbol("lambda"), ScmPair(_parsed_args ,(macro_expand body)))), _parsed_vals)
            | _ -> raise (X_syntax_error(sexp, "Let should have args and body") )
       

    and generate_whatever = function
    | ScmPair(var,ScmPair(value,ScmNil))-> ScmPair(var,ScmPair(ScmPair(ScmSymbol "quote",ScmPair(ScmSymbol "whatever",ScmNil)),ScmNil)) 
    | _->raise X_not_implemented

    and generate_set = function
    | x-> ScmPair(ScmSymbol "set!", x)

    and expand_letrec sexp = 
        match sexp with
        | ScmPair (ScmNil,seq) -> macro_expand (ScmPair(ScmSymbol "let", ScmPair(ScmNil, seq)))
        | ScmPair(vars,body)-> 
                    let args = scm_map generate_whatever vars in
                    let set_vars = scm_map generate_set vars in
                    let new_body = scm_append set_vars body in
                    macro_expand (ScmPair(ScmSymbol "let", ScmPair(args ,new_body)))

(*---- End Auxilairy macro-expenders ------------------------------------------------------------------------------- *)
end;;