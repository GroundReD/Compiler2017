(**************************)
(*    syntax              *)
(**************************)
type inst = 
  | Push of int 
  | Add
  | Mul
  | Sub
  | True
  | False
  | Eq
  | Le
  | And
  | Neg
  | Fetch of var
  | Store of var 
  | Noop
  | Branch of cmd * cmd
  | Loop of cmd * cmd
  | Read
  | Print

and cmd = inst list
and var = string

(**************************)
(*    pretty printer      *)
(**************************)
let rec i2s inst =
  match inst with
  | Push n -> "push("^string_of_int n^")"
  | Add -> "add"
  | Mul -> "mul"
  | Sub -> "sub"
  | True -> "true"
  | False -> "false"
  | Eq -> "eq"
  | Le -> "le"
  | And -> "and"
  | Neg -> "neg"
  | Fetch x -> "fetch("^x^")"
  | Store x -> "store("^x^")"
  | Noop -> "noop"
  | Branch (c1,c2) -> "branch("^c2s c1^","^c2s c2^")"
  | Loop (c1,c2) -> "loop("^c2s c1^","^c2s c2^")"
  | Read -> "read"
  | Print -> "print"
and c2s cmd = List.fold_left (fun s i -> s ^ " " ^ i2s i) "" cmd
let pp cmd = print_endline (c2s cmd)

(**************************)
(*    semantics           *)
(**************************)
type value = Z of int | T of bool
type stack = value list
type state = (var, int) PMap.t

let state_empty = PMap.empty
let state_lookup x s = PMap.find x s
let state_bind x v s = PMap.add x v s

let string_of_value v 
= match v with
  | Z n -> print_endline("Z " ^ string_of_int n)
  | T b -> (
    match b with
    | true -> print_endline("T true")
    | false -> print_endline("T false")
  )

let value_Z v = 
    match v with 
    | Z n -> n
    | T _ -> raise (Failure "not integer type")

  let value_T v =
    match v with
    | T b -> b
    | Z _ -> raise (Failure "not bool type")

let next : inst list * stack * state -> inst list * stack * state
=fun (c,e,s) -> 
  (* let () = List.iter string_of_value e in *)
  (* print_endline (i2s (List.hd c)); *)
  match c with
  | Push n::c'  -> (c', (Z n)::e, s)
  | Add::c'     -> 
  (
    match e with
    | v1::v2::e' -> (c', (Z (value_Z v1 + value_Z v2))::e', s)
    | _ -> raise (Failure "not add in m")
  )
  | Mul::c'     -> 
  (
    match e with
    | v1::v2::e' -> (c', (Z (value_Z v1 * value_Z v2))::e', s)
    | _ -> raise (Failure "not mul in m")
  )
  | Sub::c'     -> 
  (
    match e with
    | v1::v2::e' -> (c', (Z (value_Z v1 - value_Z v2))::e', s)
    | _ -> raise (Failure "not sub in m")
  )
  | True::c'    -> (c', (T true)::e, s)
  | False::c'   -> (c', (T false)::e, s)
  | Eq::c'      -> 
  (
    match e with
    | v1::v2::e' -> (c', (T (value_Z v1 == value_Z v2))::e', s)
    | _ -> raise (Failure "not eq in m")
  )
  | Le::c'      -> 
  (
    match e with
    | v1::v2::e' -> (c', (T (value_Z v1 <= value_Z v2))::e', s)
    | _ -> raise (Failure "not le in m")
  )
  | And::c'     ->
  (
    match e with
    | v1::v2::e' -> (c', (T (value_T v1 && value_T v2))::e', s)
    | _ -> raise (Failure "not and in m")
  ) 
  | Neg::c'     -> 
  (
    match e with
    | v1::e' -> (c', (T (not (value_T v1)))::e', s)
    | _ -> raise (Failure "not neg in m")
  ) 
  | Fetch x::c' -> (c', (Z (state_lookup x s))::e, s )
  | Store x::c' -> 
  (
    match e with
    | v1::e' -> (c', e', state_bind x (value_Z v1) s)
    | _ -> raise (Failure "not store in m")
  ) 
  | Noop::c'    -> (c', e, s) 
  | Branch (c1, c2)::c' -> 
  (
    match e with
    | v1::e' ->  if (value_T v1) then (c1@c', e', s) else (c2@c', e', s)
    | _ -> raise (Failure "not branch in m")
  ) 
  | Loop (c1, c2)::c'   ->
    let c3 = c2@[Loop (c1, c2)] in
    let c4 = [Branch (c3, [Noop])] in 
    let c5 = c1@c4 in
    let c6 = c5@c' in 
      (c6, e , s)
  | Read::c' -> (c',(Z (read_int()))::e, s)
  | Print::c' -> 
  (
    match e with
    | v1::e' ->  print_endline (string_of_int(value_Z v1)); (c', e', s)
    | [] -> raise (Failure "empty print in m")
    | _ -> raise (Failure "not print in m")
  ) 
  

let run : cmd -> state
=fun c -> 
  let iconf = (c, [], state_empty) in
  let rec iter (c,e,s) = 
    match next (c,e,s) with
    | [], _, s  -> s
    | c', e',s' -> iter (c',e',s') in
    iter iconf
