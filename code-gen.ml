#use "semantic-analyser.ml";;
exception X_this_should_not_happen;;

(* This module is here for you convenience only!
   You are not required to use it.
   you are allowed to change it. *)
module type CODE_GEN = sig
  (* This signature assumes the structure of the constants table is
     a list of key-value pairs:
     - The keys are constant values (Sexpr(x) or Void)
     - The values are pairs of:
       * the offset from the base const_table address in bytes; and
       * a string containing the byte representation (or a sequence of nasm macros)
         of the constant value
     For example: [(Sexpr(Nil), (1, "T_NIL"))]
   *)
  val make_consts_tbl : expr' list -> (sexpr * (int * string)) list

  (* This signature assumes the structure of the fvars table is
     a list of key-value pairs:
     - The keys are the fvar names as strings
     - The values are the offsets from the base fvars_table address in bytes
     For example: [("boolean?", 0)]
   *)  
  val make_fvars_tbl : expr' list -> (string * int) list

  (* If you change the types of the constants and fvars tables, you will have to update
     this signature to match: The first argument is the constants table type, the second 
     argument is the fvars table type, and the third is an expr that has been annotated 
     by the semantic analyser.
   *)
  val generate : (sexpr * (int * string)) list -> (string * int) list -> expr' -> string
end;;




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




(**================= Auxilairy ======================================================== *)
let counter = ref 0 ;;

let rec rmv_duplicates_sexpr_lst lst =
  match lst with
  |[] -> []
  |hd::tl -> hd::(rmv_duplicates_sexpr_lst (List.filter (fun x -> if (sexpr_eq x hd) then false else true) tl));;

let rec rmv_duplicates lst =
  match lst with
  |[] -> []
  |hd::tl -> hd::(rmv_duplicates (List.filter (fun x -> if (x=hd) then false else true) tl));;
(**================= Auxilairy ======================================================== *)


(**================= Const Table functions ======================================================== *)
let rec find_consts expr const_lst = 
  match expr with 
  |ScmConst'(ScmPair(car,cdr)) -> const_lst@((find_consts (ScmConst' car) [])@(find_consts (ScmConst' cdr) []))@[ScmPair(car, cdr)]
  |ScmConst'(ScmSymbol sym) ->  const_lst@[ScmString(sym)]@[ScmSymbol sym];
  |ScmConst' (ScmVector v) ->  const_lst@(List.flatten (List.map (fun vi -> find_consts (ScmConst' vi) []) v)@[ScmVector v])
  |ScmConst' c -> const_lst@[c]
  |ScmIf'(test,_then,_else) -> const_lst@(find_consts test [])@(find_consts _then [])@(find_consts _else [])  
  |ScmSeq' lst -> (const_lst@List.flatten (List.map (fun x -> find_consts x []) lst))
  |ScmOr' lst -> (const_lst@List.flatten (List.map (fun x -> find_consts x []) lst))
  |ScmSet'(var,value) |ScmDef'(var,value) |ScmBoxSet'(var,value) -> (const_lst@List.flatten (List.map (fun x -> find_consts x []) [(ScmVar' var); value]))
  |ScmApplic'(op,seq) |ScmApplicTP'(op,seq) -> (const_lst@List.flatten (List.map (fun x -> find_consts x []) (op::seq)))
  |ScmLambdaSimple'(_,body) | ScmLambdaOpt' (_,_,body)-> find_consts body const_lst
  |_-> const_lst;;

let build_const_table const_lst = 

  let rec main const_lst const_table_acc =
    match const_lst with 
    |[] -> const_table_acc
    |hd::tl -> 
      let new_const_table_acc = (build_const_member hd const_table_acc) in
      let new_const_table_acc = (const_table_acc@new_const_table_acc) in
      (main tl new_const_table_acc)

  and build_const_member const const_table = 
    let offset = make_offset const_table in
    let asm_str = get_asm_str const const_table in
    [(const, (offset, asm_str))]

  and make_offset const_table =
    let last_const_member = get_last_member const_table in 
    let _TAG_SIZE = 1 in 
    let _Q_WORD = 8 in
    match last_const_member with
    |[sexpr, (prev_offset, _)] ->
    (
      match sexpr with
        |ScmNil |ScmVoid -> prev_offset + _TAG_SIZE
        |ScmChar _ |ScmBoolean _ -> prev_offset + _TAG_SIZE + 1
        |ScmNumber(ScmRational(_,_)) |ScmPair(_,_) -> prev_offset + _TAG_SIZE + _Q_WORD + _Q_WORD 
        |ScmNumber(ScmReal _) |ScmSymbol _  -> prev_offset + _TAG_SIZE + _Q_WORD
        |ScmString s -> prev_offset + _TAG_SIZE + _Q_WORD + (String.length s)
        |ScmVector v -> prev_offset + _TAG_SIZE + _Q_WORD + (List.length v)*_Q_WORD
    )
    |[] -> 0
    |_ -> raise X_this_should_not_happen
    (**Closure? ScmClosure?  *)
  
  and get_last_member const_table = 
    match const_table with
      |[] -> []
      |[(_, (_, _))] -> const_table
      |hd::tl -> (get_last_member tl)
      
    and find_offset_by_sub_sexpr const_sexpr const_table = 
      match const_table with
      |[] -> 0
      |(sexpr_hd, (offset_hd, _))::tl ->
        if (sexpr_eq const_sexpr sexpr_hd) then offset_hd else (find_offset_by_sub_sexpr const_sexpr tl)

  and get_asm_str const const_table =

    match const with 
        |ScmVoid -> "db T_VOID"  
        |ScmNil -> "db T_NIL"
        |ScmChar c -> String.concat "" ["MAKE_LITERAL_CHAR (";(string_of_int (int_of_char c));")"] 
        |ScmBoolean true -> "db T_BOOL, 1"
        |ScmBoolean false -> "db T_BOOL, 0"
        |ScmNumber (ScmRational (numerator,denumerator)) -> String.concat "" ["MAKE_LITERAL_RATIONAL(";(string_of_int numerator);",";(string_of_int denumerator);")"]
        |ScmNumber (ScmReal f) -> String.concat "" ["MAKE_LITERAL_FLOAT(";(string_of_float f);")"]  
        |ScmString s -> String.concat "" ["MAKE_LITERAL_STRING \"";s;"\" "]
        |ScmSymbol sym -> String.concat "" ["MAKE_LITERAL_SYMBOL(const_tbl+";(string_of_int (find_offset_by_sub_sexpr (ScmString sym) const_table));")"]
        |ScmPair (car,cdr) -> String.concat "" ["MAKE_LITERAL_PAIR(const_tbl+";(string_of_int (find_offset_by_sub_sexpr car const_table));", const_tbl+";(string_of_int (find_offset_by_sub_sexpr cdr const_table));")"]
        |ScmVector lst -> if (List.length lst == 0) then "MAKE_LITERAL_VECTOR"
                          else "MAKE_LITERAL_VECTOR " ^ String.concat ", " (List.map (fun e -> "const_tbl+"^(string_of_int (find_offset_by_sub_sexpr e const_table))) lst)
        
        in
  (main const_lst [])

;;

(**================= Const Table functions ======================================================== *)

(**================= Free var Table functions ======================================================== *)
let rec find_fVars expr fVar_lst = 
  match expr with 
  |ScmVar'(VarFree x) -> fVar_lst@[x]
  |ScmIf'(test,_then,_else) -> fVar_lst @  (find_fVars test []) @ (find_fVars _then []) @ (find_fVars _else []) 
  |ScmSeq' lst -> (fVar_lst@List.flatten (List.map (fun x -> find_fVars x []) lst))
  |ScmOr' lst -> (fVar_lst@List.flatten (List.map (fun x -> find_fVars x []) lst))
  |ScmSet'(var,value)| ScmDef'(var,value) -> fVar_lst @ (find_fVars (ScmVar' var) []) @ (find_fVars value []) 
  |ScmApplic'(op,seq) | ScmApplicTP'(op,seq) -> fVar_lst @ (find_fVars op []) @List.flatten (List.map (fun x -> find_fVars x []) seq)
  |ScmLambdaSimple'(_,body) | ScmLambdaOpt' (_,_,body)-> fVar_lst @ (find_fVars body [])
  |ScmBoxSet'(var,value) -> fVar_lst @ (find_fVars value [])
  |_->fVar_lst;;

  let elementary_procedures = [
    "*"; "+"; "-"; "/"; "<";  "="; ">";
    "append"; "apply"; "boolean?"; "car"; "cdr";
    "char->integer"; "char?"; "cons"; "cons*";
    "denominator"; "eq?"; "equal?"; "exact->inexact"; "flonum?"; "fold-left"; "fold-right"; "gcd"; 
    "integer?"; "integer->char"; "length"; "list"; "list?"; "make-string"; "map";
    "not"; "null?"; "number?"; "numerator"; "pair?"; "procedure?"; "rational?"; "set-car!"; "set-cdr!";
    "string->list"; "string-length"; "string-ref";  "string-set!";  "string?"; "symbol?"; "symbol->string"; "zero?";
  ];;

  let rec build_fvars_table fVar_lst = 
    let _Q_WORD = 8 in
    (List.mapi (fun i x -> (x,i*_Q_WORD))) (elementary_procedures @ fVar_lst);; 

  let rec find_offset_by_var_name fvar_name fvar_tbl = 
    match fvar_tbl with 
    |[] -> -1
    |(hd, offset)::tl ->
        if (fvar_name=hd) then offset else (find_offset_by_var_name fvar_name tl);;


(**================= Free var Table functions ======================================================== *)


(**================= Code Generator functions ======================================================== *)
  let rec generator consts fvars e depth = 

      generate_for_each_e consts fvars e depth


    and generate_for_each_e consts fvars e depth =
      let name_of_expr' = (fun e -> "\n;Start of sub expr \n;" ^ (string_of_expr' e) ^ "\n\n" ) in
      match e with 
        (**Const *)
        |ScmConst' c -> "mov rax, const_tbl+" ^ (string_of_int (find_offset_by_const_sexpr c consts)) ^ "\n"

        (**Vars *)
        |ScmVar'(VarParam (_,minor)) -> "mov rax, qword [rbp + 8 * (4 + " ^ (string_of_int minor) ^")]\n"
        |ScmSet'(VarParam(_, minor),epsilon) -> 
          let parsed_epsilon = generate_for_each_e consts fvars epsilon depth in 
          let store_rax = "mov qword [rbp + 8 * (4 + " ^ (string_of_int minor) ^")], rax\n" in
          let store_dummy_val = "mov rax, SOB_VOID_ADDRESS\n" in
          parsed_epsilon ^ store_rax ^ store_dummy_val
        
        |ScmVar'(VarBound (_,major,minor)) ->
          String.concat "" ["mov rax, qword[rbp+8*2]\t\t;rax<-env\n";
                                      "mov rax,qword[rax+8*";(string_of_int major);"]\t\t;rax<-rib (=major)\n";
                                      "mov rax,qword[rax+8*";(string_of_int minor);"]\t\t;rax<-rib (=minor)\n"]

        |ScmSet'(VarBound (_, major, minor), epsilon) -> 
          let parsed_epsilon = generate_for_each_e consts fvars epsilon depth in 
          let operations = String.concat "\n" ["mov rbx, qword [rbp+8*2]";
                                            "mov rbx, qword [rbx+8*";(string_of_int major);"]";
                                            "mov qword [rbx+8*";(string_of_int minor);"], rax";
                                            "mov rax, SOB_VOID_ADDRESS\n"] in
          parsed_epsilon ^ operations
        
        |ScmVar'(VarFree var) -> "mov rax, qword [fvar_tbl+" ^ (string_of_int (find_offset_by_var_name var fvars)) ^ "]\n"
        |ScmSet'(VarFree var,epsilon) -> 
          let parsed_epsilon = generate_for_each_e consts fvars epsilon depth in 
          let derefrenced = "[fvar_tbl+" ^ (string_of_int (find_offset_by_var_name var fvars)) ^ "]" in
          let store_rax = "mov qword " ^ derefrenced ^ ",rax\n" in
          let store_dummy_val = "mov rax, SOB_VOID_ADDRESS\n" in
          parsed_epsilon ^ store_rax ^ store_dummy_val


        (**Sequences *)
        |ScmSeq' seq -> (String.concat "\n" (List.map (fun expr -> generate_for_each_e consts fvars expr depth) seq)) ^ "\n"
        |ScmOr'  seq ->  
          let parsed_seq = List.map (fun expr -> generate_for_each_e consts fvars expr depth) seq in 
          let unique_lexit = (make_unique_label "Lexit")() in
          let to_add_between = "cmp rax, SOB_FALSE_ADDRESS" ^ "\n" ^ "jne " ^ unique_lexit ^ "\n" in  
          (String.concat to_add_between parsed_seq) ^ unique_lexit ^ ":\n"

        (**If expression *)
        |ScmIf'(test,_then,_else) ->
          let parsed_test = generate_for_each_e consts fvars test depth in 
          let parsed_then = generate_for_each_e consts fvars _then depth in 
          let parsed_else = generate_for_each_e consts fvars _else depth in 
          let (unique_lexit, unique_lelse) = (make_unique_labels "Lexit" "Lelse")() in
          let parsed_test = parsed_test ^ "\ncmp rax, SOB_FALSE_ADDRESS" ^ "\n" ^ "je " ^ unique_lelse in  
          let parsed_then = parsed_then ^ "\njmp " ^ unique_lexit ^ "\n" ^ unique_lelse ^ ":" in 
          let parsed_else = parsed_else ^ "\n" ^ unique_lexit ^ ":\n" in 
          String.concat "\n" [name_of_expr' e; parsed_test; parsed_then; parsed_else]

        (**Boxes *)
        |ScmBox' v ->
          let pointer = "MALLOC rcx, 8*1   ;allocate memory to box param\n" in (**8 bytes for each array cell, 1 for array of size 1 *)
          let parsed_var_in_rax = generate_for_each_e consts fvars (ScmVar' v) depth in
          let set_val = "mov [rcx], qword rax\n" in
          let mov_to_rax = "mov rax, rcx\n" in
          pointer ^ parsed_var_in_rax ^ set_val ^ mov_to_rax 

        |ScmBoxGet' v ->  
          let parsed_var_in_rax = generate_for_each_e consts fvars (ScmVar' v) depth in
          let mov_to_rax = "mov rax, qword [rax]\n" in
          parsed_var_in_rax ^ mov_to_rax
        
        |ScmBoxSet' (v, value) ->  
          let parsed_val_in_rax = generate_for_each_e consts fvars value depth in
          let push_rax = "push rax\n" in
          let parsed_var_in_rax = generate_for_each_e consts fvars (ScmVar' v) depth in
          let pop_n_dereference_rax = "pop qword [rax]\n" in 
          let store_dummy_val = "mov rax, SOB_VOID_ADDRESS\n" in
          parsed_val_in_rax ^ push_rax ^ parsed_var_in_rax ^ pop_n_dereference_rax ^ store_dummy_val

        (**Define expression *)
        |ScmDef' (VarFree var, value) -> 
          let parsed_value = generate_for_each_e consts fvars value depth in 
          let derefrenced = "[fvar_tbl+" ^ (string_of_int (find_offset_by_var_name var fvars)) ^ "]" in
          let store_rax = "mov qword " ^ derefrenced ^ ",rax\n" in
          let store_dummy_val = "mov rax, SOB_VOID_ADDRESS\n" in
          parsed_value ^ store_rax ^ store_dummy_val

        (**Lambdas *)
        |ScmLambdaSimple'(params,body) -> 
          let (unique_lcode, unique_lcont) = (make_unique_labels "Lcode" "Lcont")() in
          let ext_env_asm = extend_env_lambda depth (unique_lcode, unique_lcont) in (**returns the string asm code *)
          
          let parsed_body = generate_for_each_e consts fvars body (depth+1) in 

          let body_asm = String.concat "\n" ["push rbp";"mov rbp,rsp";parsed_body; "leave";"ret"; unique_lcont ^ ":\n"; ""] in

          (name_of_expr' e) ^ ext_env_asm ^ unique_lcode ^":\n" ^ body_asm 

        |ScmLambdaOpt'(params, opt, body) -> 
          let (unique_lcode, unique_lcont) = (make_unique_labels "Lcode" "Lcont")() in
          let ext_env_asm = extend_env_lambda depth (unique_lcode, unique_lcont) in (**returns the string asm code *)
          let parsed_body = generate_for_each_e consts fvars body (depth+1) in 
          let fixed_stack = fix_stack params in
          let body_asm = String.concat "\n" ["push rbp";"mov rbp,rsp";parsed_body; "leave";"ret"; unique_lcont ^ ":\n"; ""] in

          (name_of_expr' e) ^ ext_env_asm ^ unique_lcode ^":\n"  ^ fixed_stack ^ body_asm 


        |ScmApplic'(op, args) -> 
          let parse_n_push_args = List.fold_right (fun curr acc -> 
                                                            let parsed_e = generate_for_each_e consts fvars curr depth in
                                                            acc ^ parsed_e ^ "\npush rax\n;pushed arg " ^ (string_of_expr' curr) ^ "\n") args "\n" in
          let push_args_num = "push " ^ (string_of_int (List.length args)) ^ "  ; number of args\n" in
          let unique_continue = (make_unique_label "continue_evaluation_AppReg")() in 
          let parse_proc    = (generate_for_each_e consts fvars op depth) ^ "\n" in
          let verify_it_is_proc = "
          cmp byte [rax], T_CLOSURE
          je " ^unique_continue^"
          mov ecx, 0
          div ecx      ; divide by zero to create exception
          "^ unique_continue ^ ":
          " in
          let push_n_call = "
          push qword [rax + TYPE_SIZE]  ; push env
          call qword [rax + TYPE_SIZE + WORD_SIZE]  ; call code
          " in
          let clean = "
          add rsp , 8*1       ; pop env
          pop rbx             ; pop arg count
          lea rsp , [rsp + 8*rbx]
          " in
          (name_of_expr' e) ^ "\n; Applic\n"  ^ parse_n_push_args ^ push_args_num ^ parse_proc ^ verify_it_is_proc ^ push_n_call ^ clean 
        
        |ScmApplicTP'(op, args) -> 
          let parse_n_push_args = List.fold_right (fun curr acc -> 
                                                            let parsed_e = generate_for_each_e consts fvars curr depth in
                                                            acc ^ parsed_e ^ "\npush rax\n;pushed arg " ^ (string_of_expr' curr) ^ "\n") args "\n" in
          let push_args_num = "push " ^ (string_of_int (List.length args)) ^ "  ; number of args\n" in
          let unique_continue = (make_unique_label "continue_evaluation_AppTP")() in 
          let parse_proc    = (generate_for_each_e consts fvars op depth) ^ "\n" in
          let verify_it_is_proc = "
          cmp byte [rax], T_CLOSURE
          je " ^unique_continue^"
          mov ecx, 0
          div ecx      ; divide by zero to create exception
          "^ unique_continue ^ ":
          " in
          let push_env_prev_ret_addr = "
          push qword [rax + TYPE_SIZE]  ; push env
          push qword [rbp + WORD_SIZE]  ; old ret addr
          " in

          let fix_stack_ = 
          "
          ; find the size of previous frame = loop counter
          mov qword r10, "^(string_of_int (List.length args))^"
          add r10, 1       ; loop is going to start from 1
          add r10, 3       ; for (n, env, ret)
          SHIFT_FRAME r10
          "
          in

          (name_of_expr' e) ^ "\n; ApplicTP\n" ^ parse_n_push_args ^ push_args_num ^ parse_proc ^ verify_it_is_proc ^ push_env_prev_ret_addr ^ fix_stack_ 
          ^ "\njmp qword [rax + TYPE_SIZE + WORD_SIZE]  ; jmp to code\n" 
        

        |_ -> "\n;;;hello"

    and find_offset_by_const_sexpr const_sexpr const_table = 
      match const_table with
      |[] -> 0
      |(sexpr_hd, (offset_hd, _))::tl ->
        if (sexpr_eq const_sexpr sexpr_hd) then offset_hd else (find_offset_by_const_sexpr const_sexpr tl)

    and fix_stack params = 
      let params_count = (string_of_int (List.length params)) in (**params without optional *)
      let (unique_exit_create_nested_pairs, unique_start_create_nested_pairs) = (make_unique_labels "exit_create_nested_pairs" "start_create_nested_pairs")() in
      let unique_fix_frame = (make_unique_label "fix_frame")() in
      let (unique_start , unique_exit) = (make_unique_labels "Start_fix_stack" "Exit_fix_stack")() in
      let unique_shift_stack_cell_down = (make_unique_label "shift_down_n_add_nil")() in
      let unique_fix_stack_pointers = (make_unique_label "fix_stack_pointers")() in
      let asm_code = 
      
      unique_start ^":   
        mov r8, qword [rsp + 8*2]     ; r8 = params number in stack (=real number)
        
        ; for each optional argument in stack (r8 - params_count) append to list
        cmp r8, " ^ params_count ^ "  ; if equals, add nil and inc argc
        je " ^ unique_shift_stack_cell_down ^ " 
        
        ; else, must loop to append all extra elements and decrease stack size
        sub r8, " ^ params_count ^ "  ; r8 = |only optional args|
        lea r9, [rsp + (WORD_SIZE * 2)]   ; r9 points to argc
        mov r10, [r9]                     ; r10 = argc
        shl r10, 3                        ; argc in bytes
        add r9, r10                       ; r9 points to the last optional argument on stack

        push r9                           ; store the last arg address

        ; we'll now loop and append from last to first all optional arguments
        mov rcx, r8                 ; loop counter = PARAM_COUNT - 'regular params'
        mov r12, SOB_NIL_ADDRESS    ; NIL is the init var
        " ^ unique_start_create_nested_pairs ^ ":
        cmp rcx, 0
        je " ^ unique_exit_create_nested_pairs ^ " 

        mov r10, qword [r9]         ; get the curr opt arg
        MAKE_PAIR(r11, r10, r12)    ; r11 = Pair(r10, r12)
        mov r12, r11                ; r12 is the pair to return = acc

        sub r9, WORD_SIZE           ; point to the next (=below) arg
        dec rcx
        jmp " ^ unique_start_create_nested_pairs ^ "

        "^unique_exit_create_nested_pairs^":

        pop r9                      ; address of A_n-1
        mov qword [r9], r12         ; put there the list we built                          
        
        ; fix argc (= regular params + 1 )
        mov r15, "^params_count^" 
        inc r15
        mov qword [rsp + 2*WORD_SIZE], r15

        mov rcx, 2                            ; (ret, env, n) => (0,1,2)
        add rcx, "^params_count^"             ; loop counter, rcx = 2 + regular params
        sub r9, WORD_SIZE                     ; r9 is the curr cell to occupy

        ; loop to fix frame
        "^unique_fix_frame^":
        mov r11, rcx
        shl r11, 3                            ; r11 = WORD_SIZE*r11
        mov r12, [rsp + r11]                  ; r12 is the the value we want to move
        mov qword [r9], r12                  ; move it to the next cell to occupy

        cmp rcx, 0
        je "^unique_fix_stack_pointers^"
        ; else update counters and continue
        sub r9, WORD_SIZE                    ; update r9: next cell to occupy
        dec rcx
        jmp "^unique_fix_frame^"
        
      "^unique_shift_stack_cell_down ^":
        ; shift one cell down = enlarge frame (updates rsp as well)
        INC_FRAME
        jmp "^unique_exit^"
        
      "^ unique_fix_stack_pointers ^ ":\n
      mov rsp, r9
      "^ unique_exit ^ ":\n
      \n      
      " 
      
      in 
      asm_code

    and extend_env_lambda depth (unique_lcode, unique_lcont) =
        match depth with
        |0 -> 
          String.concat "\n" ["MAKE_CLOSURE(rax, SOB_NIL_ADDRESS, " ^ unique_lcode ^ ")" ; "jmp " ^ unique_lcont ; ""]
        |_ ->
          let extEnv = "MALLOC rbx, (1+" ^ (string_of_int depth) ^ ")*8" ^ "\n" in (* (1+|ENV|)*8 *)
          let curr_env_ptr = "mov rdx, qword [rbp+8*2]     ; rdx points to curr lexical env\n" in
          let unique_start_loop = (make_unique_label "loop")() in
          let (unique_start_loop2, unique_end_loop2) = (make_unique_labels "loop" "end_loop")() in
          let loop = String.concat "\n" [
            "mov rcx, 0" ;
            "mov r8, " ^ (string_of_int depth) ;
            unique_start_loop ^ ":" ;
            "mov r9, qword [rdx + rcx*8]" ;
            "mov qword [rbx + (rcx+1)*8], r9    ; ext_env[i+1] <- lex_env[i]" ;
            "inc rcx" ;
            "cmp rcx, r8    ; are we finished?" ;
            "jne " ^ unique_start_loop ^ "    ; if not, repeat" ;
            ""  ;
          ] in
          let loop2 = String.concat "\n" [
            "; now copy current params" ;
            "mov r8,  qword [rbp + (8*3)]   ; r8=len(params)" ;
            "shl r8, 3        ; number of bytes" ;
            "MALLOC rdx, r8   ; accumulate params in rdx" ;
            "shr r8, 3        ; restore number of params" ;
            "mov rcx, 0       ; set loop counter" ;
            unique_start_loop2 ^ ":"  ;
            "cmp rcx, r8" ;
            "je " ^ unique_end_loop2 ;
            "mov r11, PVAR(rcx)\n"  ;
            "mov qword [rdx + 8*(rcx)], r11"  ;
            "inc rcx" ;
            "jmp " ^ unique_start_loop2 ;

            unique_end_loop2 ^ ":"  ;
            "mov qword [rbx], rdx   ; rbx is the pointer to ExtEnv, rbx[0] <- rdx"  ;
            "MAKE_CLOSURE(rax, rbx, " ^ unique_lcode ^ ")" ;
            "jmp " ^ unique_lcont ; 
            ""
          ] in
          extEnv ^ curr_env_ptr ^ loop ^ loop2

      (**ExtEnv= allocate |ENV|+1 memory *)
      (**copy ENV to ExtEnv[1]...ExtEnv[ENV] *)
      (**ExtEnv[0]= allocate len(params) memory *)
      (**put all params in this vector *)
      (**allocate the closure object= [env, code] *)
      (**set rax->env to point to the new ExtEnv *)
      (**set rax->code to point to the body of this closure *)
      (**jmp to next command= Lcont_i  for i the counter of labels*) 

    and make_unique_label label = 
      fun () -> ( counter := !counter + 1; Printf.sprintf "%s%d" label (!counter) )
    
    and make_unique_labels label1 label2 = 
      fun () -> ( counter := !counter + 1; (Printf.sprintf "%s%d" label1 (!counter), Printf.sprintf "%s%d" label2 (!counter)) )


;;

(**================= Code Generator functions ======================================================== *)


module Code_Gen : CODE_GEN = struct
  let make_consts_tbl asts = 
    let basic_consts = [ScmVoid; ScmNil; ScmBoolean false; ScmBoolean true] in
    let found_consts = List.flatten(List.map (fun e -> (find_consts e [])) asts) in (**for each e' retreive consts and accumulate*) 
    let found_consts = basic_consts@found_consts in
    let found_consts = rmv_duplicates_sexpr_lst found_consts in
    let const_table = build_const_table found_consts in
    const_table  
  ;;
  
  
  let make_fvars_tbl asts = 
    let free_var_list = List.flatten(List.map (fun e -> (find_fVars e [])) asts) in 
    let fVar_without_duplicates = rmv_duplicates free_var_list in
    let fVar_with_offset = (build_fvars_table fVar_without_duplicates) in
    fVar_with_offset;;


  let generate consts fvars e = "\n\n\n;START OF CODE of "^(string_of_expr' e)^"\n\n\n" ^ (generator consts fvars e 0)  (**0 for depth*)
  ;;
end;;

