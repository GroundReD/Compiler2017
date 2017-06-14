open T
(* TODO: Implement an optimizer *)
(* (***********************************)
(* abstract syntax definition of T *)
(***********************************)

type program = linstr list
and linstr = label * instr (* labeled instruction *)
and instr = 
  | SKIP
  | ALLOC of var * int  (* x = alloc(n) *)
  | ASSIGNV of var * bop * var * var (* x = y bop z *)
  | ASSIGNC of var * bop * var * int (* x = y bop n *)
  | ASSIGNU of var * uop * var  (* x = uop y *)
  | COPY of var * var           (* x = y *)
  | COPYC of var * int          (* x = n *)
  | UJUMP of label              (* goto L *)
  | CJUMP of var * label        (* if x goto L *)
  | CJUMPF of var * label       (* ifFalse x goto L *)
  | LOAD of var * arr           (* x = a[i] *)
  | STORE of arr * var          (* a[i] = x *)
  | READ of var                 (* read x *)
  | WRITE of var                (* write x *)
  | HALT
and var = string
and label = int
and arr = var * var
and bop = ADD | SUB | MUL | DIV | LT | LE | GT | GE | EQ | AND | OR
and uop = MINUS | NOT

let dummy_label = 0
 *)


let print_llinst : T.linstr -> unit
= fun (lb, inst) -> 
	let pn = print_endline in 
	let s_bop o = match o with ADD -> "+" | SUB -> "-" | MUL -> "*" | DIV -> "/"| LT -> "<" | LE -> "<=" | GT -> ">" | GE -> ">=" | EQ -> "==" | AND -> "&&" | OR -> "||" in
  	let s_uop o = match o with MINUS -> "-" | NOT -> "!" in
  	let s_arr (x,y) = x ^ "[" ^ y ^ "]" in
	match inst with
    | HALT -> pn (string_of_int lb^" : HALT")
    | SKIP -> pn (string_of_int lb^" : SKIP")
    | ALLOC (x,n) -> pn (string_of_int lb^" : "^x ^ " = alloc (" ^ string_of_int n ^ ")")
    | ASSIGNV (x,o,y,z) -> pn (string_of_int lb^" : "^x ^ " = " ^ y ^ " " ^ s_bop o ^ " " ^ z)
    | ASSIGNC (x,o,y,n) -> pn (string_of_int lb^" : "^x ^ " = " ^ y ^ " " ^ s_bop o ^ " " ^ string_of_int n)
    | ASSIGNU (x,o,y) -> pn (string_of_int lb^" : "^x ^ " = " ^ s_uop o ^ y)
    | COPY (x,y) -> pn (string_of_int lb^" : "^x ^ " = " ^ y)
    | COPYC (x,n) -> pn (string_of_int lb^" : "^x ^ " = " ^ string_of_int n)
    | UJUMP label -> pn (string_of_int lb^" : "^"goto " ^ string_of_int label)
    | CJUMP (x,l) -> pn (string_of_int lb^" : "^"if " ^ x ^ " goto " ^ string_of_int l)
    | CJUMPF (x,l) -> pn (string_of_int lb^" : "^"iffalse " ^ x ^ " goto " ^ string_of_int l)
    | LOAD (x,a) -> pn (string_of_int lb^" : "^x ^ " = " ^ s_arr a)
    | STORE (a,x) -> pn (string_of_int lb^" : "^s_arr a ^  " = " ^ x)
    | READ x -> pn (string_of_int lb^" : "^"read " ^ x)
    | WRITE x -> pn (string_of_int lb^" : "^"write " ^ x)

let eval_bin : int -> bop -> int -> int
=fun v1 op v2 ->
  match v1,op,v2 with
  |  n1, ADD,  n2 ->  (n1+n2)
  |  n1, SUB,  n2 ->  (n1-n2)
  |  n1, MUL,  n2 ->  (n1*n2)
  |  n1, DIV,  n2 ->  (n1/n2)
  |  n1, LT,  n2 -> if n1 < n2 then  1 else  0
  |  n1, LE,  n2 -> if n1 <= n2 then 1 else  0
  |  n1, GT,  n2 -> if n1 > n2 then  1 else  0
  |  n1, GE,  n2 -> if n1 >= n2 then  1 else  0
  |  n1, EQ,  n2 -> if n1 = n2 then  1 else  0
  |  n1, AND, n2 -> if n1 != 0 && n2 != 0 then  1 else 0
  |  n1, OR,  n2 -> if n1 != 0 || n2 != 0 then  1 else 0
  | _ -> raise (Failure "T.eval_binary : invalid operands")

let eval_una : uop -> int -> int
=fun op v ->
  match op, v with
  | MINUS, n -> (-n)
  | NOT,  n -> if n = 0 then 1 else 0
  | _ -> raise (Failure "T.eval_unary: invalid operands")


(* let is_copy_prop 
= fun linlist u v b->
	let tmp_list = [] in 
	for i = 0 to (List.length linlist) do
		let linst = List.nth linlist i in
		let lb = fst linst in
		let inst = snd linst in
		match inst with
		| ASSIGNV (x, o, y, z) ->  if (* x = y bop z *)
		| ASSIGNC (x, o, y, n) -> (* x = y bop n *)
		| ASSIGNU (x, o, y) -> (* x = uop y *)
		| COPY (x, y) ->            (* x = y *)
		| COPYC (x, n) ->           (* x = n *)
		| UJUMP lb ->              (* goto L *)
		| CJUMP (x, lb) ->        (* if x goto L *)
		| CJUMPF (x, lb) ->       (* ifFalse x goto L *)
		| LOAD (x, a) ->           (* x = a[i] *)
		| STORE (a, x) ->          (* a[i] = x *)
		| READ x ->                 (* read x *)
		| WRITE x ->                 (* write x *)
		| _ -> tmp_list@linst
	done; tmpList *)

let check_redf = ref false

let rec is_copy_prop : T.linstr list -> string -> string -> bool -> T.linstr list
= fun linlist u v b-> 
	(* print_endline("========================="); *)
	(* print_endline("is copy"); *)
	(* List.iter print_llinst linlist; *)
	(* print_endline("========================="); *)
	match linlist with
	| [] -> []
	| hd::tl -> (
		(* print_endline (u^" "^v ); *)
		(* print_endline("copy check "^string_of_bool !check_redf); *)
		(* print_llinst hd; *)
		if !check_redf then hd::tl
		else
			match hd with
			| (lb, ASSIGNV (x, o, y, z)) ->  
				if u = x then let _ = check_redf := true in hd::tl
				else if (u = y && u = z) then 
					let _ = check_redf := false in 
					let l1 = (lb, ASSIGNV (x, o, v, v))::tl in
					let l2 = is_copy_prop l1 u v !check_redf in   
						l2
				else if u = y then 
					let _ = check_redf := false in 
					let l1 = (lb, ASSIGNV (x, o, v, z))::tl in
					let l2 = is_copy_prop l1 u v !check_redf in   
						l2
				else if u = z then 
					let _ = check_redf := false in 
					let l1 = (lb, ASSIGNV (x, o, y, v))::tl in
					let l2 = is_copy_prop l1 u v !check_redf in   
						l2
				else 
					hd::(is_copy_prop tl u v !check_redf) 		(* x = y bop z *)
			| (lb, ASSIGNC (x, o, y, n)) -> 
				if u = x then let _ = check_redf := true in hd::tl
				else if u = y then 
					let _ = check_redf := false in 
					let l1 = (lb, ASSIGNC (x, o, v, n))::tl in
					let l2 = is_copy_prop l1 u v !check_redf in   
						l2
				else 
					hd::(is_copy_prop tl u v !check_redf)		(* x = y bop n *)
			| (lb, ASSIGNU (x, o, y)) -> 
				if u = x then let _ = check_redf := true in hd::tl
				else if u = y then 
					let _ = check_redf := false in 
					let l1 = (lb, ASSIGNU (x, o, v))::tl in
					let l2 = is_copy_prop l1 u v !check_redf in   
						l2
				else 
					hd::(is_copy_prop tl u v !check_redf)		(* x = uop y *)
			| (lb, COPY (x, y)) -> 
				(* print_endline (string_of_bool (u=y)); *)
				(* print_endline (u^" "^v); *)
				(* print_llinst hd; *)
				if u = x then let _ = check_redf := true in hd::tl
				else if u = y then 
					let _ = check_redf := false in 
					let l1 = (lb, COPY (x, v))::tl in
					let l2 = is_copy_prop l1 u v !check_redf in   
						l2
				else 
					hd::(is_copy_prop tl u v !check_redf)           (* x = y *)
			| (lb, COPYC (x, n)) ->
				if u = x then let _ = check_redf := true in hd::tl
				else hd::(is_copy_prop tl u v !check_redf)           (* x = n *)
			| (lb, CJUMP (x, l)) ->
				if u = x then 
					let _ = check_redf := false in 
					let l1 = (lb, CJUMP (v, l))::tl in
					let l2 = is_copy_prop l1 u v !check_redf in   
						l2
				else 
					hd::(is_copy_prop tl u v !check_redf)     (* if x goto L *)
			| (lb, CJUMPF (x, l)) ->
				if u = x then 
					let _ = check_redf := false in 
					let l1 = (lb, CJUMPF (v, l))::tl in
					let l2 = is_copy_prop l1 u v !check_redf in   
						l2
				else 
					hd::(is_copy_prop tl u v !check_redf)        (* ifFalse x goto L *)        (* x = a[i] *)
			| (lb, STORE (a, x)) ->
				if u = x then 
					let _ = check_redf := false in 
					let l1 = (lb, STORE (a, v))::tl in
					let l2 = is_copy_prop l1 u v !check_redf in   
						l2
				else 
					hd::(is_copy_prop tl u v !check_redf)            (* a[i] = x *)
			| (lb, READ x) -> 
				if u = x then 
					let _ = check_redf := false in 
					let l1 = (lb, READ v)::tl in
					let l2 = is_copy_prop l1 u v !check_redf in   
						l2
				else 
					hd::(is_copy_prop tl u v !check_redf)                  (* read x *)
			| (lb, WRITE x) ->
				if u = x then 
					let _ = check_redf := false in 
					let l1 = (lb, WRITE v)::tl in
					let l2 = is_copy_prop l1 u v !check_redf in   
						l2
				else 
					hd::(is_copy_prop tl u v !check_redf)                  (* write x *)
			| _ -> hd::(is_copy_prop tl u v !check_redf)
	)


let rec copy_props : T.linstr list -> T.linstr list
= fun linlist ->
	(* print_endline("copy props"); *)
	match linlist with
	| [] -> []
	| hd::tl -> (
		let _ = check_redf := false in 
		match hd with
		| (lb ,COPY(x, y)) -> hd::(copy_props (is_copy_prop tl x y !check_redf))
		| _ -> hd::(copy_props tl)
	)


let check_redf_cons_fold = ref false

let rec is_const_fold : T.linstr list -> string -> int -> bool -> T.linstr list
= fun linlist u num b-> 
	(* print_endline("is const"); *)
	(* List.iter print_llinst linlist; *)
	(* print_endline ( u^" "^string_of_int num); *)
	match linlist with
	| [] -> []
	| hd::tl -> (
		if !check_redf_cons_fold then hd::tl
		else
			match hd with
			| (lb, ASSIGNV (x, o, y, z)) -> 
				(* print_endline (string_of_bool (u=y)); *)
				(* print_endline (u^" "^string_of_int num); *)
				(* print_llinst hd;  *)
				if u = x then let _ = check_redf_cons_fold := true in hd::tl
				else if (u = y && u = z) then 
					let _ = check_redf_cons_fold := false in 
					let l1 = (lb,  COPYC (x, (eval_bin num o num)))::tl in
					let l2 = is_const_fold l1 u num !check_redf_cons_fold in   
						l2
				else if u = z then 
					let _ = check_redf_cons_fold := false in 
					let l1 = (lb, ASSIGNC (x, o, y, num))::tl in
					let l2 = is_const_fold l1 u num !check_redf_cons_fold in   
						l2
				else 
					hd::(is_const_fold tl u num !check_redf_cons_fold) 		(* x = y bop z *)
			| (lb, ASSIGNC (x, o, y, n)) -> 
				(* print_endline (string_of_bool (u=y)); *)
				(* print_endline (u^" "^string_of_int num); *)
				(* print_llinst hd; *)
				if u = x then let _ = check_redf_cons_fold := true in hd::tl
				else if u = y then 
					let _ = check_redf_cons_fold := false in 
					let l1 = (lb, COPYC (x, (eval_bin num o n)))::tl in
					let l2 = is_const_fold l1 u num !check_redf_cons_fold in   
						l2
				else 
					hd::(is_const_fold tl u num !check_redf_cons_fold)		(* x = y bop n *)
			| (lb, ASSIGNU (x, o, y)) -> 
				(* print_endline (string_of_bool (u=y)); *)
				(* print_endline (u^" "^string_of_int num); *)
				(* print_llinst hd; *)
				if u = x then let _ = check_redf_cons_fold := true in hd::tl
				else if u = y then 
					let _ = check_redf_cons_fold := false in 
					let l1 = (lb, COPYC (x, eval_una o num))::tl in
					let l2 = is_const_fold l1 u num !check_redf_cons_fold in   
						l2
				else 
					hd::(is_const_fold tl u num !check_redf_cons_fold)		(* x = uop y *)
			| (lb, COPY (x, y)) -> 
				(* print_llinst hd; *)
				(* print_endline (string_of_bool (u=y)); *)
				(* print_endline (u^" "^string_of_int num); *)
				if u = x then let _ = check_redf_cons_fold := true in hd::tl
				else if u = y then 
					let _ = check_redf_cons_fold := false in 
					let l1 = (lb, COPYC (x, num))::tl in
					let l2 = is_const_fold l1 u num !check_redf_cons_fold in   
						l2
				else 
					hd::(is_const_fold tl u num !check_redf_cons_fold)           (* x = y *)
			| (lb, COPYC (x, n)) ->
				(* print_endline (u^" "^string_of_int num); *)
				(* print_llinst hd; *)
				if u = x then let _ = check_redf_cons_fold := true in hd::tl
				else hd::(is_const_fold tl u num !check_redf_cons_fold)           (* x = n *)           (* a[i] = x *)
			| _ -> hd::(is_const_fold tl u num !check_redf_cons_fold)
	)



let rec const_fold : T.linstr list -> T.linstr list
= fun linlist ->
	(* print_endline("contant folding"); *)
	match linlist with
	| [] -> []
	| hd::tl -> (
		(* print_llinst hd; *)
		let _ = check_redf_cons_fold := false in
		match hd with
		| (lb ,COPYC(x, n)) -> hd::(const_fold (is_const_fold tl x n !check_redf_cons_fold))
		| _ -> hd::(const_fold tl)
	)

let check_redf_dead = ref false
let check_jump = ref false

let rec is_dead : T.linstr list -> string -> bool -> bool
= fun linlist u b-> 
	(* print_endline("==========================="); *)
	(* print_endline("is dead"); *)
	(* List.iter print_llinst linlist; *)
	(* print_endline("==========================="); *)
	match linlist with
	| [] -> true
	| hd::tl -> (
		(* print_endline u; *)
		(* print_endline("dead check "^string_of_bool !check_redf_dead); *)
		(* print_llinst hd; *)
		if !check_redf_dead then !check_redf_dead 
		else 
			match hd with
			| (lb, ASSIGNV (x, o, y, z)) ->  
				if u = x then let _ = check_redf_dead := true in !check_redf_dead
				else if u = y then let _ = check_redf_dead := false in !check_redf_dead
				else if u = z then let _ = check_redf_dead := false in !check_redf_dead
				else (is_dead tl u !check_redf_dead) 		(* x = y bop z *)
			| (lb, ASSIGNC (x, o, y, n)) -> 
				if u = x then let _ = check_redf_dead := true in !check_redf_dead
				else if u = y then let _ = check_redf_dead := false in !check_redf_dead
				else (is_dead tl u !check_redf_dead) 		(* x = y bop n *)
			| (lb, ASSIGNU (x, o, y)) -> 
				if u = x then let _ = check_redf_dead := true in !check_redf_dead
				else if u = y then let _ = check_redf_dead := false in !check_redf_dead
				else (is_dead tl u !check_redf_dead) 		(* x = uop y *)
			| (lb, COPY (x, y)) -> 
				if u = x then let _ = check_redf_dead := true in !check_redf_dead
				else if u = y then let _ = check_redf_dead := false in !check_redf_dead
				else (is_dead tl u !check_redf_dead)           (* x = y *)
			| (lb, COPYC (x, n)) ->
				if u = x then let _ = check_redf_dead := true in !check_redf_dead
				else (is_dead tl u !check_redf_dead)           (* x = n *)
		             (* goto L *)
			| (lb, CJUMP (x, l)) ->
				if u = x then let _ = check_redf_dead := false in !check_redf_dead
				else (is_dead tl u !check_redf_dead)   (* if x goto L *)
			| (lb, CJUMPF (x, l)) ->
				if u = x then let _ = check_redf_dead := false in !check_redf_dead
				else (is_dead tl u !check_redf_dead)     (* ifFalse x goto L *)        
			| (lb, LOAD (x, (a1, a2))) ->
				if u = x then let _ = check_redf_dead := false in !check_redf_dead
				else if u = a1 then let _ = check_redf_dead := false in !check_redf_dead
				else if u = a2 then let _ = check_redf_dead := false in !check_redf_dead
				else (is_dead tl u !check_redf_dead)
				(* x = a[i] *)
			| (lb, STORE ((a1, a2), x)) ->
				if u = x then let _ = check_redf_dead := false in !check_redf_dead
				else if u = a1 then let _ = check_redf_dead := false in !check_redf_dead
				else if u = a2 then let _ = check_redf_dead := false in !check_redf_dead
				else (is_dead tl u !check_redf_dead)           (* a[i] = x *)
			| (lb, READ x) -> 
				if u = x then let _ = check_redf_dead := false in !check_redf_dead
				else (is_dead tl u !check_redf_dead)                 (* read x *)
			| (lb, WRITE x) ->
				if u = x then let _ = check_redf_dead := false in !check_redf_dead
				else (is_dead tl u !check_redf_dead)                   (* write x *)
			| _ -> is_dead tl u !check_redf_dead
		)	
	
let rec dead_code_elim : T.linstr list -> string list ref -> T.linstr list
= fun linlist ori_list->
	(* print_endline("======================="); *)
	(* print_endline("dead_code_elim"); *)
	(* List.iter print_llinst linlist; *)
	match linlist with
	| [] -> []
	| hd::tl -> (
		let _ = check_redf_dead := false in
		match hd with
		| (lb, COPY (x, y) ) -> 
		if not (List.mem x !ori_list) then (
			if (is_dead tl x !check_redf_dead) then 
				(* let _ = print_endline ((string_of_int lb)^" : "^x^" dead") in let _ = print_llinst((lb, snd (List.hd tl))) in  *)
				if lb = 0 then dead_code_elim tl ori_list
				else dead_code_elim ((lb, snd (List.hd tl))::(List.tl tl)) ori_list 
			else let _ = ori_list := !ori_list@[x] in hd::(dead_code_elim tl ori_list)
		)
		else hd::(dead_code_elim tl ori_list)

		| (lb, COPYC (x, n)) -> 
		if not (List.mem x !ori_list) then (
			if (is_dead tl x !check_redf_dead) then 
				(* let _ = print_endline ((string_of_int lb)^" : "^x^" dead") in let _ = print_llinst((lb, snd (List.hd tl))) in  *)
				if lb = 0 then dead_code_elim tl ori_list
				else dead_code_elim ((lb, snd (List.hd tl))::(List.tl tl)) ori_list 
			else let _ = ori_list := !ori_list@[x] in hd::(dead_code_elim tl ori_list)
		)
		else hd::(dead_code_elim tl ori_list)
		| _ -> hd::(dead_code_elim tl ori_list)
	)

let rec get_leaders : T.linstr list -> T.linstr list -> T.linstr list
= fun linlist blist->
	(* List.iter print_llinst blist; *)
	(* (* print_endline("===========leaders================="); *) *)
	(* raise (Failure "optimizer: Not implemented") *)
	match linlist with
	| [] -> blist
	| (lb, inst)::tl-> (
		match (lb, inst) with
		| (0, inst) -> (
			match inst with
			| UJUMP l -> let new_blist = blist@[List.hd tl] in get_leaders (List.tl tl) new_blist
			| CJUMP (v,l) -> let new_blist = blist@[List.hd tl] in get_leaders (List.tl tl) new_blist
			| CJUMPF (v, l) -> let new_blist = blist@[List.hd tl] in get_leaders (List.tl tl) new_blist
			| _ -> get_leaders tl blist
		)
		| (_, _) -> let new_blist = blist@[(lb, inst)] in get_leaders tl new_blist
	)


exception Failure of string

let rec find x lst =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if x = h then 0 else 1 + find x t

let rec get_leaders_pos 
= fun leaders code ->
	match leaders with
	| [] -> []
	| hd::tl -> (
		(find hd code)::(get_leaders_pos tl code)
	)

	
let rec split_block 
= fun linlist poslist->
	let tmp = ref [] in 
	let block_tmp = ref [] in
	let add_l l = tmp := !tmp @[l] in
	let add_block b = block_tmp := !block_tmp@[b] in 

	for i = 0 to List.length poslist -1 do
		let _ = tmp := [] in 
		if i = (List.length poslist - 1) then 
		begin
			for j = List.nth poslist i to List.length linlist -1 do
				add_l (List.nth linlist j)
			done;
			add_block !tmp
		end

		else
			begin
			 	for j = List.nth poslist i  to (List.nth poslist (i+1))-1 do
			 		add_l (List.nth linlist j)
			 	done;
			 	add_block !tmp
			 end 
		

	done; !block_tmp
	


let get_basic_block : T.linstr list -> (T.linstr list) list
= fun linlist ->
	let block_list = ref [List.hd linlist] in
	let tmp = get_leaders linlist !block_list in 
	let pos_l = get_leaders_pos tmp linlist in 
	let tmp_block = split_block linlist pos_l in 
		print_endline(string_of_int (List.length tmp_block));
		List.iter print_llinst (List.hd tmp_block) ;
		tmp_block


let rec mv_skip_label : T.linstr list -> T.linstr list
= fun linlist ->
	match linlist with
	| [] -> []
	| hd::tl -> (
		match hd with
		| (lb, SKIP) -> mv_skip_label ((lb, snd (List.hd tl))::(List.tl tl))
		| (_, _) -> hd::(mv_skip_label tl)
	)

let optimize_block : T.linstr list -> T.linstr list
= fun linlist ->
	let code1 = copy_props linlist in 
		(* print_endline("--------------optimize------------");
		List.iter print_llinst code1; *)
	let code2 = const_fold code1 in
		(* print_endline("--------------optimize------------");
		List.iter print_llinst code2; *)
		code2
	
let rec optimize_blocks : (T.linstr list) list -> T.linstr list
= fun linlist_list ->
	match linlist_list with
	| [] -> []
	| hd::tl -> (
		(* print_endline("--------------code-----------");
		List.iter print_llinst hd; *)
		let block1 = optimize_block hd in 
		let blocks = optimize_blocks tl in
			block1@blocks
	)

let optimize_all : T.linstr list -> T.linstr list
= fun linlist ->
	let code_list = get_basic_block linlist in
	let op_code = optimize_blocks code_list in
	let ori_list = ref [] in 
	let elim_code = dead_code_elim op_code ori_list in
		(* op_code *)
	let code_list2 = get_basic_block elim_code in 
	let op_code2 = optimize_blocks code_list2 in
	let ori_list2 = ref [] in 
	let elim_code2 = dead_code_elim op_code2 ori_list2 in

	let code_list3 = get_basic_block elim_code2 in 
	let op_code3 = optimize_blocks code_list3 in
	let ori_list3 = ref [] in 
	let elim_code3 = dead_code_elim op_code3 ori_list3 in

	let code_list4 = get_basic_block elim_code3 in 
	let op_code4 = optimize_blocks code_list4 in
	let ori_list4 = ref [] in 
	let elim_code4 = dead_code_elim op_code4 ori_list4 in

		elim_code4

let optimize : T.program -> T.program
= fun t -> 	let init_code = mv_skip_label t in 
			let opti_code = optimize_all init_code in
				opti_code