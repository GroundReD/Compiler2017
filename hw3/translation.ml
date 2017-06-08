open While
open M

let rec trans_a : While.aexp -> M.inst list
= fun a ->
	match a with
	| NUM n -> [Push n] 
  	| VAR x -> [Fetch x]
  	| ADD (a1, a2) -> (trans_a a2)@(trans_a a1)@[Add]
  	| SUB (a1, a2) -> (trans_a a2)@(trans_a a1)@[Sub]
  	| MUL (a1, a2) -> (trans_a a2)@(trans_a a1)@[Mul]
	| _ -> raise (Failure ("trans_a not implemented"))

let rec trans_b : While.bexp -> M.inst list
= fun b ->
	match b with
	| TRUE  -> [True]
 	| FALSE -> [False]
  	| EQ (a1, a2)	-> (trans_a a2)@(trans_a a1)@[Eq]
  	| LE (a1, a2)   -> (trans_a a2)@(trans_a a1)@[Le]
 	| AND (b1, b2)  -> (trans_b b2)@(trans_b b1)@[And]
  	| NEG b' -> (trans_b b')@[Neg]
	| _ -> raise (Failure ("trans_b not implemented"))	


let rec trans : stmt -> inst list
=fun c -> 
	match c with
	| ASSIGN (x, a) -> (trans_a a)@[Store x]
  	| SKIP -> [Noop]
  	| SEQ (c1, c2) -> (trans c1)@(trans c2)
  	| IF (b, c1, c2) -> (trans_b b)@[Branch (trans c1, trans c2)]
 	| WHILE (b, c) -> [Loop (trans_b b, trans c)]
	| READ x -> [Read]@[Store x]
 	| PRINT e -> (trans_a e)@[Print]
	| _ -> raise (Failure ("trans state not implemented"))
