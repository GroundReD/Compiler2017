open S
open T

let t_num = ref (-1)
let label = 0
let label_num = ref 0

let add_one : int ref -> int
= fun v -> let _ =  v := !v+1 in !v 

let create_t = fun () -> let n = add_one t_num in (* print_endline("create"^string_of_int(n)); *)"t"^string_of_int(n)
let create_l = fun () -> let n = add_one label_num in n

let rec trans_exp : S.exp -> T.var * T.linstr list 
= fun e ->
 (*  print_endline("trans_exp");
  p_exp(e);
  print_endline(" "); *)

	match e with
	| NUM n -> let t = create_t() in ( t, [ (label, COPYC(t,n)) ] )
  | LV lv -> (
    match lv with
    	| ID x -> let t = create_t() in ( t, [ (label, COPY(t,x)) ] )
    	| ARR (x, e') -> 
        let t2 = create_t() in
        let (t1, c) = trans_exp e' in 
    	 		(t2, c@[ ( label, LOAD(t2, (x, t1) ) )] )
    	)
  | ADD (e1, e2) ->
    let t3 = create_t() in
    let (t1, c1) = trans_exp e1 in
    let (t2, c2) = trans_exp e2 in 
    	(t3, c1@c2@[label, ASSIGNV (t3, ADD, t1, t2)] )
 	| SUB (e1, e2) ->
    let t3 = create_t() in
 		let (t1, c1) = trans_exp e1 in
    let (t2, c2) = trans_exp e2 in 
    	(t3, c1@c2@[label, ASSIGNV (t3, SUB, t1, t2)] )
  | MUL (e1, e2) ->
 		let t3 = create_t() in
    let (t1, c1) = trans_exp e1 in
    let (t2, c2) = trans_exp e2 in 
    	(t3, c1@c2@[label, ASSIGNV (t3, MUL, t1, t2)] )
  | DIV (e1, e2) ->
    let t3 = create_t() in
 		let (t1, c1) = trans_exp e1 in
    let (t2, c2) = trans_exp e2 in 
    	(t3, c1@c2@[label, ASSIGNV (t3, DIV, t1, t2)] )
  | MINUS e' ->
 		let (t1, c) = trans_exp e' in
    let t2 = create_t() in
    	(t2, c@[label, ASSIGNU (t2, MINUS, t1)] )         
  | NOT e' ->
    let t2 = create_t() in
 		let (t1, c) = trans_exp e' in
    	(t2, c@[label, ASSIGNU (t2, NOT, t1)] )         
  | LT (e1, e2) ->
    let t3 = create_t() in
 		let (t1, c1) = trans_exp e1 in
    let (t2, c2) = trans_exp e2 in 
    	(t3, c1@c2@[label, ASSIGNV (t3, LT, t1, t2)] )
  | LE (e1, e2) ->
    let t3 = create_t() in
 		let (t1, c1) = trans_exp e1 in
    let (t2, c2) = trans_exp e2 in 
   
    	(t3, c1@c2@[label, ASSIGNV (t3, LE, t1, t2)] )
  | GT (e1, e2) ->
    let t3 = create_t() in
 		let (t1, c1) = trans_exp e1 in
    let (t2, c2) = trans_exp e2 in 
   
    	(t3, c1@c2@[label, ASSIGNV (t3, GT, t1, t2)] )
  | GE (e1, e2) ->
    let t3 = create_t() in
   	let (t1, c1) = trans_exp e1 in
    let (t2, c2) = trans_exp e2 in 
    	(t3, c1@c2@[label, ASSIGNV (t3, GE, t1, t2)] )
  | EQ (e1, e2) ->
    let t3 = create_t() in
 		let (t1, c1) = trans_exp e1 in
    let (t2, c2) = trans_exp e2 in 
    	(t3, c1@c2@[label, ASSIGNV (t3, EQ, t1, t2)] )
  | AND (e1, e2) ->
    let t3 = create_t() in
 	  let (t1, c1) = trans_exp e1 in
    let (t2, c2) = trans_exp e2 in 
    	(t3, c1@c2@[label, ASSIGNV (t3, AND, t1, t2)] )
  | OR  (e1, e2) ->
    let t3 = create_t() in
 		let (t1, c1) = trans_exp e1 in
    let (t2, c2) = trans_exp e2 in 
    	(t3, c1@c2@[label, ASSIGNV (t3, OR, t1, t2)] )

let rec trans_stmt : S.stmt -> T.linstr list
= fun st ->
  (* print_endline("trans_stmt"); *)
  (* p_stmt 0 st; print_endline(""); *)
	match st with
	| ASSIGN (lv, e1) -> (
		match lv with
		| ID x -> let (t, c) = trans_exp e1 in c@[(label, COPY (x, t))]
		| ARR (x, e2) -> 
			let (t1, c1) = trans_exp e1 in
			let (t2, c2) = trans_exp e2 in
				c1@c2@[ (label, STORE( (x,t2), t1 )) ]
	)
	| IF (e,st1, st2) ->
    let (t, c) = trans_exp e in
    let c_true = trans_stmt st1 in
    let c_false = trans_stmt st2 in
    let lb_t = create_l() in
    let lb_f = create_l() in
    let lb_x = create_l() in
      c@[(label, CJUMP(t, lb_t))]@[(label, UJUMP(lb_f))]@[(lb_t,SKIP)]@
      c_true@[(label, UJUMP(lb_x))]@[(lb_f, SKIP)]@
      c_false@[(label, UJUMP(lb_x))]@[(lb_x, SKIP)]
  | WHILE (e, st) ->
    let (t, c) = trans_exp e in
    let c_b = trans_stmt st in
    let lb_e = create_l() in
    let lb_x = create_l() in
      [(lb_e, SKIP)]@c@[(label, CJUMPF(t, lb_x))]@c_b@[(label, UJUMP(lb_e))]@[(lb_x, SKIP)]
  | DOWHILE (st,e) ->
      (trans_stmt st)@(trans_stmt (WHILE(e,st)))
  | READ x -> [(label, READ x)]
  | PRINT e -> let (t, c) = trans_exp e in c@[(label, WRITE t)]
  | BLOCK b -> trans_block b

and trans_stmts : S.stmts -> T.linstr list
= fun sts -> 
  (* print_endline("trans_stmts"); *)
  match sts with
  | [] -> []
  | hd::tl -> 
    (trans_stmt hd)@(trans_stmts tl)

and trans_decl : S.decl -> T.linstr
= fun (t, id) -> 
  (* print_endline("trans_decl"); *)
  (* p_decl 0 (t,id); print_endline(""); *)
  match t with
  | TINT -> (label, COPYC(id, 0))
  | TARR n -> (label, ALLOC(id, n))

and trans_decls : S.decls -> T.linstr list
= fun decs ->
  (* print_endline("trans_decls"); *)
  match decs with
  | [] -> []
  | hd::tl -> 
    (trans_decl hd)::(trans_decls tl)

and trans_block : S.block -> T.linstr list
= fun (decs, sts) -> 
  (* print_endline("trans_block"); *)
  (trans_decls decs)@(trans_stmts sts)

let translate : S.program -> T.program
= fun s -> (trans_block s)@[(label, HALT)]

