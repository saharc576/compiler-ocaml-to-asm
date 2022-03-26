#use "pc.ml";;

let rec gcd a b =
  match (a, b) with
  | (0, b) -> b
  | (a, 0) -> a
  | (a, b) -> gcd b (a mod b);;

type scm_number =
  | ScmRational of (int * int)
  | ScmReal of float;;

type sexpr =
  | ScmVoid
  | ScmNil
  | ScmBoolean of bool
  | ScmChar of char
  | ScmString of string
  | ScmSymbol of string
  | ScmNumber of scm_number
  | ScmVector of (sexpr list)
  | ScmPair of (sexpr * sexpr);;

module type READER = sig
    val nt_sexpr : sexpr PC.parser
end;; (* end of READER signature *)

module Reader : READER = struct
open PC;;


(*************************************  Auxialiry Parsers   ***************************************************** *)
let unitify nt = pack nt (fun _ -> ());;
let _hashtag_ = (char '#');;
let _lp_ = (char '(');;
let _rp_ = (char ')');;
let _plus_ = (char '+');;
let _minus_ = (char '-');;
let _fraction_ = (char '/');;
let _backslash_ = (char '/');;
let _mul_ = (char '*');;
let _expo_ = (char '^');;
let _dot_ = (char '.');;
let _curBR_ = (char '}');;
let _curBL_ = (char '{');;
let _digit_ = (range '0' '9');;
let _hLLetter_ =  (range 'a' 'f');;
let _hCLetter_ =  (range 'A' 'F');; 
let _exclamationMark_=(char '!');;
let _dollar_=(char '$');;
let _underscore_ = (char '_');;
let _question_ = (char '?');;
let _underscore_ = (char '_');;
let _colon_ = (char ':');;
let _equal_ = (char '=');;
let _greater_ = (char '>');;
let _less_ = (char '<');;
let _quote_ = (char '\'');;
let _quasiQuote_ = (char '`');;
let _comma_ = (char ',');;
let _at_ = (char '@');;
let _abcChar_ = (range_ci 'a' 'z');;

(*Special chars*)
let _newline_ = pack (word_ci "newline") (fun _ -> char_of_int 10);;
let _nul_ = pack (word_ci "nul") (fun _ -> char_of_int 0);;
let _page_ = pack (word_ci "page") (fun _ -> char_of_int 12);;
let _return_ = pack (word_ci "return") (fun _ -> char_of_int 13);;
let _space_ = pack (word_ci "space") (fun _ -> char_of_int 32);;
let _tab_ = pack (word_ci "tab") (fun _ -> char_of_int 9);;
let _doubleQuote_ = pack (char '\"')(fun _->char_of_int 34);;
let _tilde_ = pack (char '~')(fun _->char_of_int 126);;
let _semiColon_ = pack (char ';')(fun _->char_of_int 59);;
let _hashtagAndSemicolon_ = word "#;" ;;

(************************************* End Auxialiry Parsers   ***************************************************** *)

let rec nt_whitespace str =
  const (fun ch -> ch <= ' ') str

and nt_end_of_line_or_file str =
  let nt1 = unitify (char '\n') in
  let nt2 = unitify nt_end_of_input in
  let nt1 = disj nt1 nt2 in
  nt1 str

  and nt_line_comment str = 
  let _end_ = disj (unitify (char '\n')) (unitify nt_end_of_input) in
  let nt1 = diff nt_any _end_ in
  let _lineComment_= unitify (caten _semiColon_ (caten (star nt1) _end_)) in
  _lineComment_ str

and nt_sexpr_comment str = 
  let _nt1_= nt_sexpr in
  let _nt2_= nt_sexpr_comment in
  let _nt3_ = unitify (caten _hashtagAndSemicolon_ _nt1_ ) in
  let _nt4_ = unitify ( disj _nt3_ (unitify (caten _hashtagAndSemicolon_ (caten _nt2_ _nt1_)))) in
_nt4_ str

and nt_paired_comment str = 
  let nt1= char '{' in
  let nt2= disj_list [unitify nt_char; unitify nt_string; unitify nt_comment] in
  let nt2'= disj nt2 (unitify (one_of "{}")) in
  let nt3=diff nt_any nt2' in
  let nt3=disj (unitify nt3) nt2 in
  let nt3=star nt3 in
  let nt4= char '}' in
  let nt1= unitify (caten nt1 (caten nt3 nt4)) in
  nt1 str

and nt_comment str =
  disj_list
    [nt_line_comment;
     nt_paired_comment;
     nt_sexpr_comment] str

and nt_skip_star str =
  let nt1 = disj (unitify nt_whitespace) nt_comment in
  let nt1 = unitify (star nt1) in
  nt1 str

and make_skipped_star (nt : 'a parser) =
  let nt1 = caten nt_skip_star (caten nt nt_skip_star) in
  let nt1 = pack nt1 (fun (_, (e, _)) -> e) in
  nt1

and nt_nil str =
  let _nil_ = pack (caten (star nt_whitespace) (caten _lp_ (caten (star nt_whitespace) (caten _rp_ (star nt_whitespace))))) (fun _ -> ScmNil) in
_nil_ str 

(*************************************  Number parser   ***************************************************** *)
and nt_natural str =
  let _zero_ = int_of_char '0' in
  let _AsciiToNum_ = pack _digit_ (fun c -> (int_of_char c) - _zero_) in
  let _loop_ = plus _AsciiToNum_ in
  let _calc_ = pack _loop_ ( fun (digits) -> List.fold_left (fun x y -> 10*x + y) 0 digits ) in
  _calc_ str

and _sign_ str = 
  let _nt_ = pack (maybe (disj _plus_ _minus_)) (fun s-> match s with 
    |Some('+')-> '+'
    |Some('-')->'-'
    | _ -> '+') in
    _nt_ str
     
and nt_int str = 
  let _integer_ = pack (caten _sign_ nt_natural) (fun (s, n) -> 
                                                      match s with 
                                                      | '+' -> n
                                                      | _ -> -1*n ) in
  _integer_ str

and nt_frac str = 
  let _numerator_ = pack (caten _sign_ nt_natural) (fun (s, n) -> 
                                                             match s with 
                                                             | '+' -> n
                                                             | _ -> -1*n ) in
  let _forwardslash_ = char '/' in
  let _denomenator_ = only_if nt_natural (fun res -> res > 0) in 
  let _rational_ = caten _numerator_ ( caten _forwardslash_ _denomenator_ ) in
  let _packed_ = pack _rational_ (fun (x, (ch, y)) -> 
                                                      (** find gcd and minimize *)
                                                      let _gcd_ = gcd x y in
                                                      let _minNumarator_ = x / _gcd_ in
                                                      let _minDenomenator_ = y / _gcd_ in
                                                      ScmRational( _minNumarator_, _minDenomenator_)
   ) in
  _packed_ str

and nt_integer_part str = 
  nt_int str

and nt_mantissa str =
  let _loop_ = plus _digit_ in
  let _floatNum_ = pack _loop_ (fun digits -> float_of_string (list_to_string (['0'; '.']@digits) )) in

  let _maybeNum_ = pack (maybe _floatNum_) (fun n -> 
                                              match n with 
                                              | None -> float_of_int 0
                                              | Some(n) -> n) in
  _maybeNum_ str

and nt_exponent str = 
  let _expToken_ = disj_list [word "e"; word "E"; word "*10^"; word "*10**"] in
  let _maybeExpToken_ = pack (maybe _expToken_) (fun w -> 
                                                        match w with 
                                                        | Some(w) -> w
                                                        | None -> ['X']) in 
  let _maybeNum_ = pack (maybe nt_int) (fun n -> 
                                              match n with 
                                              | None -> 1
                                              | Some(n) -> n) in
  let _power_ = pack (caten _maybeExpToken_ _maybeNum_ ) (fun (e_sign, n) -> [e_sign, n]) in
  _power_ str

and nt_float str = 
  let _10_ = float_of_int 10 in 
  let _floatASpecial_ = pack ( caten ( caten  (caten _minus_ _dot_) nt_mantissa ) nt_exponent ) 
    (fun ( ( (_minus_, dot), mantissa), expo) ->
    (** in this case there was a minus before the decimal dot, so the mantissa should be negative *) 
    let _man_ =  -1.*.mantissa in
      match expo with 
      | [['X'], _] -> 
    begin
        let _e_ = float_of_int 0 in 
        let _num_ =  _man_ *. _10_**_e_ in
        ScmReal(_num_)
    end
      | [_, n] ->
    begin
        let _e_ = float_of_int n in 
        let _num_ =  _man_ *. _10_**_e_ in
        ScmReal(_num_) 
    end
    ) in

  let _floatA_ = pack ( caten ( caten  (caten nt_integer_part _dot_) nt_mantissa ) nt_exponent ) 
    (fun ( ( (int_part, dot), mantissa), expo) ->
      let _f_int_ = float_of_int int_part in 
      let _man_ = if(_f_int_<0. ) then -1.*.mantissa else mantissa in
      match expo with 
      | [['X'], _] -> 
      begin
          let _e_ = float_of_int 0 in 
          let _num_ = (_f_int_ +. _man_) *. _10_**_e_ in
          ScmReal(_num_)
      end
      | [_, n] ->
      begin
          let _e_ = float_of_int n in 
          let _num_ = (_f_int_ +. _man_) *. _10_**_e_ in
          ScmReal(_num_) 
      end
    ) in

  let _floatB_ = pack ( caten ( caten _dot_ nt_mantissa ) nt_exponent ) 
  (fun ( (dot, mantissa), expo) ->
      (
        match expo with 
        | [['X'], _] -> 
          (
            match mantissa with 
            |0.0 -> raise X_no_match
            |_ ->
                let _e_ = float_of_int 0 in 
                let _num_ = mantissa *. _10_**_e_ in
                ScmReal(_num_) 
          ) 
        | [_, n] -> 
          let _e_ = float_of_int n in 
          let _num_ = mantissa *. _10_**_e_ in
          ScmReal(_num_) 
      )
  ) in 

  let _floatC_ = pack ( caten nt_integer_part nt_exponent ) 
  (fun (n, expo) ->

    match expo with 
    | [['X'], _] -> 
    begin
      raise X_no_match
    end
    | [_, n_] ->
    begin
      let _e_ = float_of_int n_ in 
      let _f_int_ = float_of_int n in 
      let _num_ = _f_int_ *. _10_**_e_ in
      ScmReal(_num_) 
    end
    ) in 

  let _float_ = disj_list [_floatASpecial_; _floatA_; _floatB_; _floatC_] in 
  _float_ str


and nt_number str =
  let nt1 = nt_float in
  let nt2 = nt_frac in
  let nt3 = pack nt_int (fun n -> ScmRational(n, 1)) in
  let nt1 = disj nt1 (disj nt2 nt3) in
  let nt1 = pack nt1 (fun r -> ScmNumber r) in
  let nt1 = not_followed_by nt1 nt_symbol_char in
  nt1 str
(*************************************  End Number parser   ***************************************************** *)

and nt_boolean str = 
  let _true_ = pack (word_ci "#t") (fun _->true) in
  let _false_ = pack (word_ci "#f") (fun _->false) in
  let _Boolean_ = disj _true_ _false_ in
  let _Boolean_ =pack _Boolean_ (fun b -> ScmBoolean b) in
  _Boolean_  str

(*************************************  Char parser   ***************************************************** *)
and nt_char_simple str = const (fun ch -> 32 < int_of_char ch) str

and make_named_char char_name ch = 
  let _packed_ = pack (word_ci char_name) (fun _ -> ch) in
  _packed_ 

and nt_char_named str =
  let nt1 =
    disj_list [(make_named_char "newline" '\n');
               (make_named_char "page" '\012');
               (make_named_char "nul" '\000');
               (make_named_char "return" '\r');
               (make_named_char "space" ' ');
               (make_named_char "tab" '\t')] in
  nt1 str

and nt_char_hex str = 
  let _hexChar_ = disj_list [(range '0' '9'); (range 'a' 'f'); (range 'A' 'F')] in
  let _prefix_ = pack (char 'x') (fun _ -> ['0'; 'x']) in
  let _hexChar_ = pack (caten _prefix_ (plus _hexChar_)) (fun (p, list_hex) -> (p@list_hex) ) in 
  let _hexChar_ = pack _hexChar_ (fun l -> char_of_int (int_of_string (list_to_string l ) )) in
  _hexChar_ str

and nt_char str = 
  let _charPrefix_ =  word "#\\" in
  let _disj_ = disj_list [nt_char_named; (not_followed_by nt_char_simple nt_symbol_char); nt_char_hex] in
  let _finChar_ = pack ( caten _charPrefix_ _disj_ ) (fun (_,c) -> ScmChar(c)) in
  _finChar_ str
(*************************************  End Char parser   ***************************************************** *)

(*************************************  Symbol parser   ***************************************************** *)
and nt_symbol_char str = 
  let _symbolChar_= disj_list[_digit_ ; _abcChar_; _exclamationMark_; _dollar_; _expo_; _mul_; _minus_; _underscore_; _equal_ ;_plus_; _greater_; _less_; _question_; _backslash_; _colon_] in
  _symbolChar_ str
  
and nt_symbol str =
  let nt1 = plus nt_symbol_char in
  let nt1 = pack nt1 list_to_string in
  let nt1 = pack nt1 (fun name -> ScmSymbol name) in
  let nt1 = diff nt1 nt_number in
  nt1 str
(*************************************  End Symbol parser   ***************************************************** *)


(*************************************  String parser   ***************************************************** *)


and nt_stringChar str= 
  let _string_char_ = disj_list [nt_stringMetaChar; nt_stringHexChar; nt_stringLiteralChar] in
  let _output_ = pack _string_char_ (fun c -> c) in
  _output_ str

and nt_stringLiteralChar str=  const (fun c -> c <> '\\' && c <> '"' && c <> '~') str

and nt_stringMetaChar str=
  let nt1 = 
      disj_list [
      (not_followed_by(make_named_char "\\\"" '"') nt_char_simple);
      (not_followed_by(make_named_char "\\n" '\n') nt_char_simple);
      (not_followed_by(make_named_char "\\t" '\t') nt_char_simple);
      (not_followed_by(make_named_char "\\f" '\012') nt_char_simple);
      (not_followed_by(make_named_char "\\r" '\r') nt_char_simple);
      (not_followed_by(make_named_char "\\\\" '\\') nt_char_simple);
      (not_followed_by(make_named_char "~~" '~') nt_char_simple)] in
  nt1 str

and nt_stringHexChar str=
  let _hexChar_ = disj (range '0' '9') (range 'a' 'f') in
  let _prefix_ = pack (word_ci "\\x") (fun _ -> ['0'; 'x']) in
  let _hexChar_ = pack ( caten (caten _prefix_ (plus _hexChar_)) _semiColon_) (fun ((p, list_hex), sem) -> (p@list_hex) ) in 
  let _stringHexChar_ = pack _hexChar_ (fun l -> char_of_int (int_of_string (list_to_string l ) )) in
  _stringHexChar_ str

and nt_str_interpolated str=
  let nt_space = star nt_whitespace in
  let nt1 = caten _tilde_ (caten _curBL_ nt_space) in
  let nt1 = pack nt1 (fun (_,(_,_))-> "~{") in
  let nt1 = caten nt1 (caten nt_sexpr (caten nt_space _curBR_)) in 
  let nt1 = pack nt1 (fun (_,(sexpr,(_,_))) -> ScmPair(ScmSymbol "format",
  ScmPair(ScmString "~a", ScmPair(sexpr, ScmNil))) ) in
  nt1 str

 and nt_string str=
  let rec list_to_proper_list = function
  | [] -> ScmNil
  | hd::tl -> ScmPair (hd, list_to_proper_list tl) in
  let str_chars = plus nt_stringChar in
  let str_chars = pack str_chars list_to_string in
  let str_chars = pack str_chars (fun x -> ScmString x) in
  let nt1 = plus (disj nt_str_interpolated str_chars) in
  let nt3 = char '\"' in
  let nt1 = caten nt3 (caten nt1 nt3) in
  let nt1 = pack nt1 (fun (_, (exp, _))-> exp) in
  let nt1 = pack nt1 (fun str_elements -> 
              match str_elements with
              | [] -> ScmString ""
              | h::[] -> h
              | h::t -> list_to_proper_list ([ScmSymbol "string-append"]@str_elements)) in
  let nt2 = (pack (word "\"\"") (fun _ -> ScmString(""))) in 
  let nt1 = disj nt1 nt2 in
  nt1 str
                   
(*************************************  End String parser   ***************************************************** *)

(*************************************   List parser   ***************************************************** *)
  
and nt_proper_list str =
  let _noParens_ = pack (caten _lp_ (caten (star nt_sexpr) _rp_)) (fun (l, (list_sexp, r)) -> 
                                                                    List.fold_right (fun x y -> ScmPair (x ,y)) list_sexp  ScmNil 
                                                                  ) in
  (disj nt_nil _noParens_ str)

and nt_improper_list str =
  let _exceptLast_ = star nt_sexpr in
  let _noParens_ = pack (caten (caten _lp_ _exceptLast_ ) (caten (caten _dot_ nt_sexpr)  _rp_) ) (fun ((l, list_sexp), ((d, lastP), r)) -> 
                                                                                match (List.length list_sexp) with
                                                                                | 0 -> lastP
                                                                                | _ -> 
                                                                                 List.fold_right (fun x y -> ScmPair (x ,y)) list_sexp  lastP 
                                                                              ) in
  _noParens_ str

and nt_list str =
  let _disj_ = disj nt_proper_list nt_improper_list in
  _disj_ str

(*************************************   End List parser   ***************************************************** *)


and nt_UnquoteAndSpliced str = 
  let nt1= caten _comma_ _at_ in
  let nt1= caten nt1 (star nt_whitespace) in 
  let nt1 = pack nt1 (fun (_) -> ",@") in
  let nt1 = caten nt1 nt_sexpr in
  let nt2 = pack nt1 (fun (_, sexpr) -> ScmPair (ScmSymbol "unquote-splicing", ScmPair (sexpr, ScmNil))) in
  nt2 str
 
 and nt_QuasiQuoted str=
  let nt1= caten _quasiQuote_ (star nt_whitespace) in 
  let nt1 = pack nt1 (fun (_) -> "`") in
  let nt1 = caten nt1 nt_sexpr in
  let nt2 = pack nt1 (fun (_, sexpr) -> ScmPair (ScmSymbol "quasiquote", ScmPair (sexpr, ScmNil))) in
  nt2 str

and nt_Unquoted str=
  let nt1= caten _comma_ (star nt_whitespace) in 
  let nt1 = pack nt1 (fun (_) -> ",") in
  let nt1 = caten nt1 nt_sexpr in
  let nt2 = pack nt1 (fun (_, sexpr) -> ScmPair (ScmSymbol "unquote", ScmPair (sexpr, ScmNil))) in
  nt2 str

and nt_Quoted str=
  let nt1= caten _quote_ (star nt_whitespace) in 
  let nt1 = pack nt1 (fun (_) -> "'") in
  let nt1 = caten nt1 nt_sexpr in
  let nt2 = pack nt1 (fun (_, sexpr) -> ScmPair (ScmSymbol "quote", ScmPair (sexpr, ScmNil))) in
  nt2 str

and nt_quoted_forms str = 
  let _disj_ = disj_list[nt_Quoted; nt_Unquoted; nt_UnquoteAndSpliced ;nt_QuasiQuoted] in
  _disj_ str

and nt_vector str =
  let nt1 = word "#(" in
  let nt2 = caten nt_skip_star (char ')') in
  let nt2 = pack nt2 (fun _ -> ScmVector []) in
  let nt3 = plus nt_sexpr in
  let nt4 = char ')' in
  let nt3 = caten nt3 nt4 in
  let nt3 = pack nt3 (fun (sexprs, _) -> ScmVector sexprs) in
  let nt2 = disj nt2 nt3 in
  let nt1 = caten nt1 nt2 in
  let nt1 = pack nt1 (fun (_, sexpr) -> sexpr) in
  nt1 str

and nt_sexpr str =
  let nt1 =
    disj_list [nt_number; nt_boolean; nt_char; nt_vector;
     nt_string; nt_list; nt_quoted_forms; nt_symbol] in
  let nt1 = make_skipped_star nt1 in 
  nt1 str;;

end;; (* end of struct Reader *)

let rec string_of_sexpr = function
  | ScmVoid -> "#<void>"
  | ScmNil -> "()"
  | ScmBoolean(false) -> "#f"
  | ScmBoolean(true) -> "#t"
  | ScmChar('\n') -> "#\\newline"
  | ScmChar('\r') -> "#\\return"
  | ScmChar('\012') -> "#\\page"
  | ScmChar('\t') -> "#\\tab"
  | ScmChar(' ') -> "#\\space"
  | ScmChar(ch) ->
     if (ch < ' ')
     then let n = int_of_char ch in
          Printf.sprintf "#\\x%x" n
     else Printf.sprintf "#\\%c" ch
  | ScmString(str) ->
     Printf.sprintf "\"%s\""
       (String.concat ""
          (List.map
             (function
              | '\n' -> "\\n"
              | '\012' -> "\\f"
              | '\r' -> "\\r"
              | '\t' -> "\\t"
              | ch ->
                 if (ch < ' ')
                 then Printf.sprintf "\\x%x;" (int_of_char ch)
                 else Printf.sprintf "%c" ch)
             (string_to_list str)))
  | ScmSymbol(sym) -> sym
  | ScmNumber(ScmRational(0, _)) -> "0"
  | ScmNumber(ScmRational(num, 1)) -> Printf.sprintf "%d" num
  | ScmNumber(ScmRational(num, -1)) -> Printf.sprintf "%d" (- num)
  | ScmNumber(ScmRational(num, den)) -> Printf.sprintf "%d/%d" num den
  | ScmNumber(ScmReal(x)) -> Printf.sprintf "%f" x
  | ScmVector(sexprs) ->
     let strings = List.map string_of_sexpr sexprs in
     let inner_string = String.concat " " strings in
     Printf.sprintf "#(%s)" inner_string
  | ScmPair(ScmSymbol "quote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "'%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "quasiquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf "`%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",%s" (string_of_sexpr sexpr)
  | ScmPair(ScmSymbol "unquote-splicing",
            ScmPair(sexpr, ScmNil)) ->
     Printf.sprintf ",@%s" (string_of_sexpr sexpr)
  | ScmPair(car, cdr) ->
     string_of_sexpr' (string_of_sexpr car) cdr
and string_of_sexpr' car_string = function
  | ScmNil -> Printf.sprintf "(%s)" car_string
  | ScmPair(cadr, cddr) ->
     let new_car_string =
       Printf.sprintf "%s %s" car_string (string_of_sexpr cadr) in
     string_of_sexpr' new_car_string cddr
  | cdr ->
     let cdr_string = (string_of_sexpr cdr) in
     Printf.sprintf "(%s . %s)" car_string cdr_string;;

let rec sexpr_eq s1 s2 =
  match s1, s2 with
  | ScmBoolean(b1), ScmBoolean(b2) -> b1 = b2
  | ScmNil, ScmNil -> true
  | ScmNumber(ScmReal f1), ScmNumber(ScmReal f2) -> abs_float(f1 -. f2) < 0.001
  | ScmNumber(ScmRational n1), ScmNumber(ScmRational n2) -> n1 = n2
  | ScmChar(c1), ScmChar(c2) -> c1 = c2
  | ScmString(s1), ScmString(s2) -> s1 = s2
  | ScmSymbol(s1), ScmSymbol(s2) -> s1 = s2
  | ScmPair(car1, cdr1), ScmPair(car2, cdr2) -> (sexpr_eq car1 car2) && (sexpr_eq cdr1 cdr2)
  | _ -> false;;