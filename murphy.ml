(* TOKENS *)

type token =

  | TOK_NUM of int (* 1234 *)
  | TOK_IDE of string (* var *)

  | TOK_IF (* TEST ... *)
  | TOK_THEN (* SUCCESS ... *)
  | TOK_ELSE (* FAIL ... *)

  | TOK_PLUS (* ...+... *)
  | TOK_MINUS (* ...-... *)
  | TOK_DIV (* .../... *)
  | TOK_MUL (* ...*... *)

  | TOK_LET (* DEF ... *)
  | TOK_IN (* IN ... *)
  | TOK_EQ (* ...=... *)

  | TOK_EQEQ (* ...==... *)
  | TOK_LESS (* ...<... *)
  | TOK_GT (* ...>... *)

  | TOK_FUNC (* FUNC ... *)
  | TOK_COMA (* , *)

  | TOK_LB (* ( *)
  | TOK_RB (* ) *)
  | TOK_EOF ;;


(* SCANNER *)

let scan code =

  let rec scanner pos linecount = 

    let code_len = String.length code
    in

    if (code_len <= pos)
    then [TOK_EOF]

    else
      let char = code.[pos]
      in

      match char with

      | ' ' -> scanner (pos + 1) linecount

      | '\r' -> if (code_len > pos + 1) 
                then 
                  (
                    match code.[pos + 1] with

                    | '\n' -> scanner (pos + 2) (linecount + 1) (* \r\n *)
                    | _ -> scanner (pos + 1) (linecount + 1) (* \n *)
                  )
                else 
                  scanner (pos + 1) (linecount + 1)

      | '\n' -> scanner (pos + 1) (linecount + 1)

      | '\t' -> scanner (pos + 1) linecount

      | 'T' -> if (code_len > pos + 4)
               then 
               (
                 match (String.sub code (pos + 1) 3, code.[pos + 4]) with

                 | ("EST", ' ') | ("EST", '\t') -> TOK_IF::(scanner (pos + 5) linecount)

                 | ("EST", '\n') -> TOK_IF::(scanner (pos + 5) (linecount + 1))

                 | ("EST", '\r') -> if (code_len > pos + 5 && code.[pos + 5] = '\n') 
                                    then 
                                      TOK_IF::(scanner (pos + 6) (linecount + 1)) (* IF \r\n *)
                                    else 
                                      TOK_IF::(scanner (pos + 5) (linecount + 1)) (* IF \r *)

                 | _ -> print_endline ("Error at line: " ^ string_of_int (linecount) ^ ".\n") ;
                        print_endline "Some possible errors: " ;
                        print_endline "1) Did you mispell TEST ? \n2) Did you try to define a variable using uppercase ?\n...\n" ;
                        failwith ("SCANNER ERROR")
               )
               else 
                failwith "Error at the end of file."

      | 'S' -> if (code_len > pos + 7)
               then 
               (
                 match (String.sub code (pos + 1) 6, code.[pos + 7]) with

                  | ("UCCESS", ' ') | ("UCCESS", '\t') -> TOK_THEN::(scanner (pos + 8) linecount)

                  | ("UCCESS", '\n') -> TOK_THEN::(scanner (pos + 8) (linecount + 1))

                  | ("UCCESS", '\r') -> if (code_len > pos + 8 && code.[pos + 8] = '\n') 
                                        then 
                                          TOK_THEN::(scanner (pos + 9) (linecount + 1)) (* SUCCESS \r\n *)
                                        else 
                                          TOK_THEN::(scanner (pos + 8) (linecount + 1)) (* SUCCESS \r *)
                 
                 | _ -> print_endline ("Error at line: " ^ string_of_int (linecount) ^ ".\n") ;
                        print_endline "Some possible errors: " ;
                        print_endline "1) Did you mispell SUCCESS ? \n2) Did you try to define a variable using uppercase ?\n...\n" ;
                        failwith ("SCANNER ERROR")
               )
               else 
                failwith "Error at the end of file."

      | 'F' -> if (code_len > pos + 4)
                then 
                (
                  match (String.sub code (pos + 1) 3, code.[pos + 4]) with

                  | ("AIL", ' ') | ("AIL", '\t') -> TOK_ELSE::(scanner (pos + 5) linecount)

                  | ("AIL", '\n') -> TOK_ELSE::(scanner (pos + 5) (linecount + 1))

                  | ("AIL", '\r') -> if (code_len > pos + 5 && code.[pos + 5] = '\n') 
                                      then 
                                        TOK_ELSE::(scanner (pos + 6) (linecount + 1)) (* FAIL \r\n *)
                                      else 
                                        TOK_ELSE::(scanner (pos + 5) (linecount + 1)) (* FAIL \r *)

                  | ("UNC", ' ') | ("UNC", '\t') -> TOK_FUNC::(scanner (pos + 5) linecount)

                  | ("UNC", '\n') -> TOK_FUNC::(scanner (pos + 5) (linecount + 1))

                  | ("UNC", '\r') -> if (code_len > pos + 5 && code.[pos + 5] = '\n') 
                                      then 
                                        TOK_FUNC::(scanner (pos + 6) (linecount + 1)) (* FUNC \r\n *)
                                      else 
                                        TOK_FUNC::(scanner (pos + 5) (linecount + 1)) (* FUNC \r *)

                  | _ -> print_endline ("Error at line: " ^ string_of_int (linecount) ^ ".\n") ;
                         print_endline "Some possible errors: " ;
                         print_endline "1) Did you mispell FAIL/FUNC ? \n2) Did you try to define a variable using uppercase ?\n...\n" ;
                         failwith ("SCANNER ERROR")
                )
                else 
                  failwith "Error at the end of file."

      | '+' -> TOK_PLUS::(scanner (pos + 1) linecount)
      | '-' -> TOK_MINUS::(scanner (pos + 1) linecount)
      | '*' -> TOK_MUL::(scanner (pos + 1) linecount)
      | '/' -> TOK_DIV::(scanner (pos + 1) linecount)

      | 'D' -> if (code_len > pos + 3)
                 then 
                 (
                   match (String.sub code (pos + 1) 2, code.[pos + 3]) with

                   | ("EF", ' ') | ("EF", '\t') -> TOK_LET::(scanner (pos + 4) linecount)

                   | ("EF", '\n') -> TOK_LET::(scanner (pos + 4) (linecount + 1))

                   | ("EF", '\r') -> if (code_len > pos + 4 && code.[pos + 4] = '\n') 
                                      then 
                                        TOK_LET::(scanner (pos + 5) (linecount + 1)) (* DEF \r\n *)
                                      else 
                                        TOK_LET::(scanner (pos + 4) (linecount + 1)) (* DEF \r *)

                   | _ -> print_endline ("Error at line: " ^ string_of_int (linecount) ^ ".\n") ;
                          print_endline "Some possible errors: " ;
                          print_endline "1) Did you mispell DEF ? \n2) Did you try to define a variable using uppercase ?\n...\n" ;
                          failwith ("SCANNER ERROR")
                 )
                 else 
                  failwith "Error at the end of file."

      | 'I' -> if (code_len > pos + 2)
                then 
                (
                  match (String.sub code (pos + 1) 1, code.[pos + 2]) with

                  | ("N", ' ') | ("N", '\t') -> TOK_IN::(scanner (pos + 3) linecount)

                  | ("N", '\n') -> TOK_IN::(scanner (pos + 3) (linecount + 1))

                  | ("N", '\r') -> if (code_len > pos + 3 && code.[pos + 3] = '\n') 
                                      then 
                                        TOK_IN::(scanner (pos + 4) (linecount + 1)) (* IN \r\n *)
                                      else 
                                        TOK_IN::(scanner (pos + 3) (linecount + 1)) (* IN \r *)

                  | _ -> print_endline ("Error at line: " ^ string_of_int (linecount) ^ ".\n") ;
                         print_endline "Some possible errors: " ;
                         print_endline "1) Did you mispell IN ? \n2) Did you try to define a variable using uppercase ?\n...\n" ;
                         failwith ("SCANNER ERROR")
                )
                else 
                  failwith "Error at the end of file."

      | '=' -> if (code_len > pos + 1) 
                then 
                (
                  match code.[pos + 1] with
                  
                  | '=' -> TOK_EQEQ::(scanner (pos + 2) linecount)
                  | _ -> TOK_EQ::(scanner (pos + 1) linecount)
                )
                else 
                  failwith "Error at the end of file."

      | '(' -> TOK_LB::(scanner (pos + 1) linecount)
      | ')' -> TOK_RB::(scanner (pos + 1) linecount)
   
      | '>' -> TOK_GT::(scanner (pos + 1) linecount)
      | '<' -> TOK_LESS::(scanner (pos + 1) linecount)
      | ',' -> TOK_COMA::(scanner (pos + 1) linecount)

      | numORide -> (
                      let rlen = code_len - pos
                      in
                      let rcode = String.sub code pos rlen
                      in 

                      match int_of_string_opt (String.make 1 numORide) with
                      
                      | None -> (
                                  let lst = ref (String.split_on_char ' ' rcode)
                                  in
                                  lst := (String.split_on_char ',' (List.hd !lst));
                                  lst := (String.split_on_char '*' (List.hd !lst));
                                  lst := (String.split_on_char '+' (List.hd !lst));
                                  lst := (String.split_on_char '/' (List.hd !lst));
                                  lst := (String.split_on_char '-' (List.hd !lst));
                                  lst := (String.split_on_char '=' (List.hd !lst));
                                  lst := (String.split_on_char '(' (List.hd !lst));
                                  lst := (String.split_on_char ')' (List.hd !lst));
                                  lst := (String.split_on_char '>' (List.hd !lst));
                                  lst := (String.split_on_char '<' (List.hd !lst));
                                  lst := (String.split_on_char '\r' (List.hd !lst));
                                  lst := (String.split_on_char '\n' (List.hd !lst));
                                  lst := (String.split_on_char '\t' (List.hd !lst));

                                  (TOK_IDE (List.hd !lst))::(scanner (pos + String.length (List.hd !lst)) linecount)
                                ) 

                      | Some _ -> (
                                  let lst = ref (String.split_on_char ' ' rcode)
                                  in
                                  lst := (String.split_on_char ',' (List.hd !lst));
                                  lst := (String.split_on_char '*' (List.hd !lst));
                                  lst := (String.split_on_char '+' (List.hd !lst));
                                  lst := (String.split_on_char '/' (List.hd !lst));
                                  lst := (String.split_on_char '-' (List.hd !lst));
                                  lst := (String.split_on_char '=' (List.hd !lst));
                                  lst := (String.split_on_char '(' (List.hd !lst));
                                  lst := (String.split_on_char ')' (List.hd !lst));
                                  lst := (String.split_on_char '>' (List.hd !lst));
                                  lst := (String.split_on_char '<' (List.hd !lst));
                                  lst := (String.split_on_char '\r' (List.hd !lst));
                                  lst := (String.split_on_char '\n' (List.hd !lst));
                                  lst := (String.split_on_char '\t' (List.hd !lst));

                                  match int_of_string_opt (List.hd !lst) with

                                  | None -> print_endline ("Error at line: " ^ string_of_int (linecount) ^ ".\n") ;
                                            print_endline "Some possible errors: " ;
                                            print_endline "1) Did you mispell a number ? \n...\n" ;
                                            failwith ("SCANNER ERROR")

                                  | Some num -> (TOK_NUM num)::(scanner (pos + String.length (List.hd !lst)) linecount)
                                ) 
                    )

  in

  scanner 0 0 ;;


(* AST *)

type exp = 

  | Num of int (* Num (123) *)
  | Ide of string (* Ide ("x") *)

  | IfTE of exp * exp * exp (* IfTE (x == 2, 5+3, 3*9) *)

  | Plus of exp * exp (* Plus (4+5, x*8) *)
  | Minus of exp * exp (* Minus (4+5, x*8) *)
  | Mul of exp * exp (* Mul (4+5, x*8) *)
  | Div of exp * exp (* Div (4+5, x*8) *)

  | EqEq of exp * exp (* EqEq (4+5, x*8) *)
  | Less of exp * exp (* Less (4+5, x*8) *)
  | Gt of exp * exp  (* Gt (4+5, x*8) *)

  | Assign of exp * exp (* Assign (x, 2*8) *)
  | Let of exp * exp (* Let (Assign (x, 2*8), x+2) *)

  | Func of exp * exp list * exp * exp (* Func (somma, [x,y], x+y, somma(1,4)) *)
  | Call of exp * exp list ;;


(* PARSER *)

let parser code =

  let tokens_list = ref (scan code)
  in

  let get_first () = if (!tokens_list == []) then failwith "Parser error" else List.hd !tokens_list
  in

  let del_first () = tokens_list := (if (!tokens_list == []) then failwith "Parser error" else List.tl !tokens_list)
  in 

  let rec assign () = 

    match get_first () with

    | TOK_IDE ide -> del_first ();
                     (
                        match get_first () with

                        | TOK_EQ -> del_first ();
                                    Assign (Ide(ide), aritm_exprLV1 ())

                        | _ -> failwith "Error: = missing in the assignment"
                     )
    | _ -> failwith "Error with the assignment"


  and cond () =

    let e1 = aritm_exprLV1 () 
    in 

    match get_first () with

    | TOK_EQEQ -> del_first ();
                  EqEq (e1, aritm_exprLV1 ()) 

    | TOK_LESS -> del_first ();
                  Less (e1, aritm_exprLV1 ()) 

    | TOK_GT -> del_first ();
                Gt (e1, aritm_exprLV1 ()) 

    | _ -> failwith "Error: ==, < or > missing in the condition" 


  and aritm_exprLV1 () =

    let e2 = aritm_exprLV2 () 
    in

    match get_first () with

    | TOK_PLUS -> del_first ();
                  Plus (e2, aritm_exprLV1 ())

    | TOK_MINUS -> del_first ();
                   Minus (e2, aritm_exprLV1 ())

    | _ -> e2 


  and aritm_exprLV2 () =

    let a = atom () 
    in

    match get_first () with

    | TOK_MUL -> del_first ();
                  Mul (a, aritm_exprLV2 ())

    | TOK_DIV -> del_first ();
                   Div (a, aritm_exprLV2 ())

    | _ -> a 

  
  and atom () =

    match get_first () with

    | TOK_LET -> del_first ();
                 let a = assign ()
                 in

                 (
                   match get_first () with

                   | TOK_IN -> del_first (); 
                               Let (a, aritm_exprLV1 ())

                   | _ -> failwith "Error: IN is missing after LET"
                 )

    | TOK_IF -> del_first ();
                let condition = cond ()
                in

                (
                  match get_first () with

                  | TOK_THEN -> del_first ();
                                let e1 = aritm_exprLV1 ()
                                in

                                (
                                  match get_first () with

                                  | TOK_ELSE -> del_first ();
                                                let e2 = aritm_exprLV1 ()
                                                in

                                                IfTE (condition, e1, e2)

                                  | _ -> failwith "Error: FAIL is missing"
                                )

                  | _ -> failwith "Error: THEN is missing"
                )

    | TOK_FUNC -> del_first ();
                  let fname = get_first ()
                  in

                  (
                    match fname with

                    | TOK_IDE fn -> del_first (); 
                                    let param_list = idelist () 
                                    in

                                    (
                                      match get_first () with

                                      | TOK_EQ -> del_first (); 
                                                  let ef = aritm_exprLV1 ()
                                                  in

                                                  (
                                                    match get_first () with

                                                    | TOK_IN -> del_first ();
                                                                Func (Ide(fn), param_list, ef, aritm_exprLV1 ())

                                                    | _ -> failwith "Error: IN is missing" 
                                                  )

                                      | _ -> failwith "Error: = is missing"
                                    )

                    | _ -> failwith "Error LET"
                  )

    | TOK_NUM num -> del_first ();
                     Num num

    | TOK_IDE ide -> del_first ();
                     if List.length !tokens_list > 0 
                     then
                      (
                        match get_first () with

                        | TOK_LB -> del_first ();
                                    let expr_list = ref [];
                                    in

                                    while List.length !tokens_list > 0 && get_first () <> TOK_RB 
                                    do 
                                      expr_list := (aritm_exprLV1 ()) :: !expr_list;
                                      if get_first () == TOK_COMA then del_first () else ();
                                    done ;

                                    if get_first () == TOK_RB then del_first () else failwith "Error: ) is missing";
                                    Call (Ide ide, !expr_list)

                        | _ -> Ide ide
                      )
                     else 
                      Ide ide

    | TOK_LB -> del_first ();
                let expr = aritm_exprLV1 () 
                in

                (
                  match get_first () with

                  | TOK_RB -> del_first ();
                              expr

                  | _ -> failwith "Error: ) is missing"
                )
    
    | _ -> failwith "Parser error"


  and idelist () =

    match get_first () with 

    | TOK_LB -> del_first ();
                (
                  let rec getList () =

                    match get_first () with
                    
                    | TOK_COMA -> del_first ();
                                  getList ()

                    | TOK_RB -> del_first ();
                                []

                    | TOK_IDE ide -> del_first ();
                                     Ide(ide)::getList()

                    | _ -> failwith "Error: param declaration failed"
                
                  in

                  getList ()
                )

    | _ -> failwith "Error: ( is missing in params declaration"

  in

  aritm_exprLV1 ();; 


(* EVALUATION TYPES *)

type evt = 

  | NUM of int
  | TRUE
  | FALSE
  | PLUS_INFINITY
  | MINUS_INFINITY
  | NAN 
  | CLOSURE of exp list * exp * ((string * evt) list) ;;

(* ENVIRONMENT *)

let env = ref [] ;;

let rec getVal ide env = 

  match env with
  | [] -> failwith (ide ^ " was not declared")
  | (el, NUM v) :: env1 -> if (String.equal el ide) then NUM v else getVal ide env1 
  | (el, CLOSURE (a,b,c)) :: env1 -> if (String.equal el ide) then CLOSURE (a,b,c) else getVal ide env1 
  | _ -> failwith "Error";;

let setVal ide v = 

  env := (ide,v) :: !env ;;

let delVal i e = 

  let rec newList ide env = 
  
    match env with
    | [] -> []
    | (el, evt) :: env1 -> if (String.equal el ide) then env1 else (el, evt) :: newList ide env1 
  in

  env := newList i e ;;


(* INTERPRETER *)

let rec eval ast =

  match ast with 

  | Num n -> NUM (n)
  | Ide ide -> (
                  let v = getVal ide !env
                  in

                  match v with

                  | NUM n -> NUM n 
                  | _ -> failwith "Error, function name in expression"
               )

  | IfTE (e1,e2,e3) -> let cond = eval e1 
                       in 

                       (
                         match cond with

                         | TRUE -> eval e2
                         | FALSE -> eval e3
                         | _ -> failwith "Error in TEST xxx SUCCESS ... FAIL ..."
                       )

  | Plus (e1,e2) -> (
                      match (eval e1, eval e2) with

                      | (NUM (n1), NUM (n2)) -> NUM (n1 + n2)
                      | (PLUS_INFINITY, MINUS_INFINITY) | (MINUS_INFINITY, PLUS_INFINITY) -> NAN
                      | (PLUS_INFINITY, _) | (_, PLUS_INFINITY) -> PLUS_INFINITY
                      | (MINUS_INFINITY, _) | (_, MINUS_INFINITY) -> MINUS_INFINITY
                      | (NAN, _) | (_, NAN) -> NAN
                      | _ -> failwith "Error" 
                    )

  | Minus (e1,e2) -> (
                        match (eval e1, eval e2) with

                        | (NUM (n1), NUM (n2)) -> NUM (n1 - n2)
                        | (PLUS_INFINITY, PLUS_INFINITY) | (MINUS_INFINITY, MINUS_INFINITY) -> NAN
                        | (PLUS_INFINITY, _) | (_, MINUS_INFINITY) -> PLUS_INFINITY
                        | (MINUS_INFINITY, _) | (_, PLUS_INFINITY) -> MINUS_INFINITY
                        | (NAN, _) | (_, NAN) -> NAN
                        | _ -> failwith "Error" 
                      )

  | Mul (e1,e2) -> (
                      match (eval e1, eval e2) with

                      | (NUM (n1), NUM (n2)) -> NUM (n1 * n2)
                      | (PLUS_INFINITY, NUM (0)) | (MINUS_INFINITY, NUM (0)) | (NUM (0), PLUS_INFINITY) | (NUM (0), MINUS_INFINITY) -> NAN
                      | (PLUS_INFINITY, PLUS_INFINITY) | (MINUS_INFINITY, MINUS_INFINITY) -> PLUS_INFINITY
                      | (MINUS_INFINITY, PLUS_INFINITY) | (PLUS_INFINITY, MINUS_INFINITY) -> MINUS_INFINITY
                      | (PLUS_INFINITY, NUM (n1)) -> if n1 > 0 then PLUS_INFINITY else MINUS_INFINITY
                      | (MINUS_INFINITY, NUM (n1)) -> if n1 < 0 then PLUS_INFINITY else MINUS_INFINITY
                      | (NAN, _) | (_, NAN) -> NAN
                      | _ -> failwith "Error" 
                    )

  | Div (e1,e2) -> (
                      match (eval e1, eval e2) with

                      | (NUM (0), NUM (0)) -> NAN 
                      | (NUM (n1), NUM (0)) -> if n1 > 0 then PLUS_INFINITY else MINUS_INFINITY
                      | (NUM (n1), NUM (n2)) -> NUM (n1 / n2)
                      | (NUM (0), _) -> NUM (0)
                      | (PLUS_INFINITY, NUM (0)) -> PLUS_INFINITY
                      | (MINUS_INFINITY, NUM (0)) -> MINUS_INFINITY
                      | (PLUS_INFINITY, PLUS_INFINITY) | (MINUS_INFINITY, MINUS_INFINITY) -> PLUS_INFINITY
                      | (MINUS_INFINITY, PLUS_INFINITY) | (PLUS_INFINITY, MINUS_INFINITY) -> MINUS_INFINITY
                      | (PLUS_INFINITY, NUM (n1)) -> if n1 > 0 then PLUS_INFINITY else MINUS_INFINITY
                      | (MINUS_INFINITY, NUM (n1)) -> if n1 < 0 then PLUS_INFINITY else MINUS_INFINITY
                      | (NUM (n1), PLUS_INFINITY) | (NUM (n1), MINUS_INFINITY) -> NUM (0)
                      | (NAN, _) | (_, NAN) -> NAN
                      | _ -> failwith "Error" 
                    )

  | EqEq (e1,e2) -> if eval e1 = eval e2 then TRUE else FALSE
  | Less (e1,e2) -> if eval e1 < eval e2 then TRUE else FALSE
  | Gt (e1,e2) -> if eval e1 > eval e2 then TRUE else FALSE

  | Let (e1,e2) -> (
                      match e1 with

                      | Assign (e3,e4) -> (
                                            match e3 with
                                            
                                            | Ide ide -> let exp = eval e4 
                                                         in
                                                         
                                                         setVal ide (match exp with | NUM (n) -> NUM (n) | _ -> failwith "Error") ;
                                                         let res = eval e2    
                                                         in

                                                         delVal ide !env;
                                                         res
                                            | _ -> failwith "Error in assignment"
                                          )
                      | _ -> failwith "Error in DEF"
                   ) 

  | Func (fn, params_lst, fb, exp) -> (
                                        match fn with

                                        | Ide name -> setVal name (CLOSURE (params_lst, fb, (name, CLOSURE (params_lst, fb, !env)) :: !env)) ;
                                                      let res = eval exp   
                                                      in

                                                      delVal name !env;
                                                      res

                                        | _ -> failwith "Error"
                                      )

  | Call (Ide fname, actuals) -> let fclos = getVal fname !env
                                  in

                                  (
                                    match fclos with

                                    | CLOSURE (formals, body, fenv) -> let ev_env = ref fenv 
                                                                        in

                                                                        if List.length formals == List.length actuals
                                                                        then
                                                                          let form = List.map (function e -> match e with | Ide ide -> ide | _ -> failwith "Error") formals
                                                                          in 

                                                                          for i = 0 to (List.length formals) - 1
                                                                          do
                                                                            ev_env := (List.nth form i, eval (List.nth actuals i)) :: !ev_env
                                                                          done 
                                                                        else
                                                                        failwith "Error: the function was not expecting this number of arguments" ;
                                                                        
                                                                        ev_env := (fname, fclos) :: !ev_env;
                                                                        let old_env = !env 
                                                                        in 
                                                                        env := !ev_env ;
                                                                        let res = eval body
                                                                        in 
                                                                        env := old_env;
                                                                        res

                                    | _ -> failwith "Error: the function was not declared."
                                  )
    
  | _ -> failwith "Error in evaluation" ;;


let print_evt exec = 

  (match exec with 
  | NUM (n) -> print_endline (string_of_int n)
  | PLUS_INFINITY -> print_endline "PLUS_INFINITY"
  | MINUS_INFINITY -> print_endline "MINUS_INFINITY"
  | NAN -> print_endline "NAN"
  | TRUE -> print_endline "TRUE" 
  | FALSE -> print_endline "FALSE"
  | _ -> print_endline "") ;;


if (Array.length Sys.argv) == 2
then 
  let ch = open_in_bin  Sys.argv.(1) 
  in

  let main = really_input_string ch (in_channel_length ch) 
  in

  close_in ch ;
  print_evt (eval (parser main)) ; 
else 
  failwith "Error: Command line arguments";;

