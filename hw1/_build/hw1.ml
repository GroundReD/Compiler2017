open Regex 

exception Not_implemented_Regex
exception Not_implemented_Nfa
exception Not_implemented_Dfa

(* let my_create_state : Nfa.t -> (Nfa.state * Nfa.t)
=fun nfa -> 
	let new_state = Nfa.create_state nfa in
	let state = fst new_state in 
	let new_nfa = snd new_state in
		(state, Nfa.add_epsilon_edge new_nfa (state,state)) *)

let string_of_set set =
  "{ " ^ (BatSet.fold (fun s str -> str ^ string_of_int s ^ ", ") set "") ^ " }"

  let string_of_states states = 
  BatSet.fold (fun s str -> str ^ string_of_set s ^ ", ")  states ""

let my_add_epsilon_edges_f : Nfa.state -> Nfa.t-> Nfa.t
= fun state_from nfa->
	let final_state = BatSet.choose (Nfa.get_final_states nfa) in
		Nfa.add_epsilon_edge nfa (state_from, final_state)

let my_add_epsilon_edges_i : Nfa.state -> Nfa.t-> Nfa.t
= fun state_from nfa->
	let init_state = Nfa.get_initial_state nfa in
		Nfa.add_epsilon_edge nfa (state_from, init_state)

let rec eval_regex :  Regex.t -> Nfa.t -> Nfa.t
=fun regex nfa ->
	match regex with
	| Empty ->
		let final_state = Nfa.create_state nfa in
		let result_nfa = Nfa.add_final_state (snd final_state) (fst final_state) in 
		(* make_self_edge result_nfa; *)
			result_nfa	
	| Epsilon ->
		let init_state = Nfa.get_initial_state nfa in 
		let new_state = Nfa.create_state nfa in
		let eps_edge = Nfa.add_epsilon_edge (snd new_state) (init_state, (fst new_state)) in
		let result_nfa = Nfa.add_final_state eps_edge (fst new_state) in
			result_nfa
  	| Alpha alpha ->
  		let init_state = Nfa.get_initial_state nfa in
  		let new_state = Nfa.create_state nfa in 
  		let edge = Nfa.add_edge (snd new_state) (init_state, alpha, (fst new_state)) in 
  		let result_nfa = Nfa.add_final_state edge (fst new_state) in
  			result_nfa
  	| OR (re1, re2) ->
  		let nfa_1 = eval_regex re1 (Nfa.create_new_nfa ()) in
  			let init_state_1 = Nfa.get_initial_state nfa_1 in 
  			let final_states_1 = Nfa.get_final_states nfa_1 in
  			let nfa_states_1 = Nfa.get_states nfa_1 in 
  			let edges_1 = Nfa.get_edges nfa_1 in

  		let nfa_2 = eval_regex re2 (Nfa.create_new_nfa ()) in 
  			let init_state_2 = Nfa.get_initial_state nfa_2 in
  			let final_states_2 = Nfa.get_final_states nfa_2 in
  			let nfa_states_2 = Nfa.get_states nfa_2 in 
  			let edges_2 = Nfa.get_edges nfa_2 in

  		let final_states = BatSet.union final_states_1 final_states_2 in

  		let init_state = Nfa.get_initial_state nfa in
  		let nfa_added_state = Nfa.add_states (Nfa.add_states nfa nfa_states_1) nfa_states_2 in
  		let nfa_added_edges = Nfa.add_edges (Nfa.add_edges nfa_added_state edges_1) edges_2 in

  		let eps_edge_1 = Nfa.add_epsilon_edge nfa_added_edges (init_state, init_state_1) in
  		let eps_edge_2 = Nfa.add_epsilon_edge eps_edge_1 (init_state, init_state_2) in

  		let final_state = Nfa.create_state eps_edge_2 in
  		let final_nfa = Nfa.add_final_state (snd final_state) (fst final_state) in

  		let result_nfa = BatSet.fold my_add_epsilon_edges_f final_states final_nfa in 
  			result_nfa  		
  	| CONCAT (re1, re2) ->
  		let nfa_1 = eval_regex re1 (Nfa.create_new_nfa ()) in
  			let init_state_1 = Nfa.get_initial_state nfa_1 in 
  			let final_states_1 = Nfa.get_final_states nfa_1 in
  			let nfa_states_1 = Nfa.get_states nfa_1 in 
  			let edges_1 = Nfa.get_edges nfa_1 in

  		let nfa_2 = eval_regex re2 (Nfa.create_new_nfa ()) in 
  			let init_state_2 = Nfa.get_initial_state nfa_2 in
  			let final_states_2 = Nfa.get_final_states nfa_2 in
  			let nfa_states_2 = Nfa.get_states nfa_2 in
  			let edges_2 = Nfa.get_edges nfa_2 in

  		let init_state = Nfa.get_initial_state nfa in
  		let nfa_added_state = Nfa.add_states (Nfa.add_states nfa nfa_states_1) nfa_states_2 in
  		let nfa_added_edges = Nfa.add_edges (Nfa.add_edges nfa_added_state edges_1) edges_2 in

  		let result_nfa_1 = Nfa.add_epsilon_edge nfa_added_edges (init_state, init_state_1) in 
  		let result_nfa_2 = (fst (BatSet.fold (fun s (nfa, s1) -> ((Nfa.add_epsilon_edge nfa (s,s1)), s1)) final_states_1 (result_nfa_1,init_state_2))) in
  		(* let result_nfa_2 = Nfa.add_epsilon_edge result_nfa_1 (BatSet.fold (fun s s' -> (s,s') final_states_1 init_state_2)) in *)
  		(* let result_nfa_2 = BatSet.fold my_add_epsilon_edges_i final_states_1 nfa_2 in *)

  		let final_state = Nfa.create_state result_nfa_2 in
  		let final_nfa = Nfa.add_final_state (snd final_state) (fst final_state) in
  		let result_nfa = BatSet.fold my_add_epsilon_edges_f final_states_2 final_nfa in
  			result_nfa
  	| STAR re ->
  		let nfa_0 = eval_regex re (Nfa.create_new_nfa()) in

  		let nfa_1 = BatSet.fold my_add_epsilon_edges_i (Nfa.get_final_states nfa_0) nfa_0 in
  			let init_state_1 = Nfa.get_initial_state nfa_1 in 
  			let final_states_1 = Nfa.get_final_states nfa_1 in
  			let nfa_states_1 = Nfa.get_states nfa_1 in 
  			let edges_1 = Nfa.get_edges nfa_1 in


  		let init_state = Nfa.get_initial_state nfa in
  		let nfa_added_state = Nfa.add_states nfa nfa_states_1 in
  		let nfa_added_edges = Nfa.add_edges nfa_added_state edges_1 in

  		let result_nfa_1 = Nfa.add_epsilon_edge nfa_added_edges (init_state, init_state_1) in 

  		let final_state = Nfa.create_state result_nfa_1 in
  		let final_nfa = Nfa.add_final_state (snd final_state) (fst final_state) in

  		let result_nfa_1 = BatSet.fold my_add_epsilon_edges_f final_states_1 final_nfa in
  		let result_nfa = Nfa.add_epsilon_edge result_nfa_1 (init_state, (fst final_state)) in
  			result_nfa

let regex2nfa : Regex.t -> Nfa.t 
=fun regex -> 
	let new_nfa = Nfa.create_new_nfa () in
	let result = eval_regex regex new_nfa in
	Nfa.print result; 
		result

let rec get_eps_closure
= fun nfa state set ->
    let next_eps_states = Nfa.get_next_state_epsilon nfa state in
    let remain_eps_state = BatSet.diff next_eps_states set in
    if BatSet.is_empty remain_eps_state then set
    else
      BatSet.fold (fun s set -> let new_set = BatSet.add s set in get_eps_closure nfa s new_set) remain_eps_state set

let get_alpha_closure
= fun nfa alpha states new_states ->
  BatSet.fold (
    fun s states ->
      let next_alpha_states = Nfa.get_next_state nfa s alpha in 
      BatSet.union next_alpha_states states
    ) states new_states

let my_add_final_state 
= fun is_final state dfa ->
    if is_final then Dfa.add_final_state dfa state
  else dfa

let my_add_eps_clousre
= fun is_mem eps_closure new_set ->
  if is_mem then new_set
else 
  BatSet.add eps_closure new_set
  

let rec eval_nfa : Nfa.t -> Dfa.t -> Dfa.state -> Dfa.states->Dfa.states -> Dfa.t
= fun nfa dfa state set_D set_W ->
        print_endline ("state " ^ string_of_set state);
        let nfa_final_state = Nfa.get_final_states nfa in
        let is_final = not (BatSet.is_empty (BatSet.intersect nfa_final_state state)) in

        let add_final_dfa = my_add_final_state is_final state dfa in

        let move_a = get_alpha_closure nfa A state (BatSet.empty) in
        print_endline ("move_a " ^ string_of_set move_a);

        let move_b = get_alpha_closure nfa B state (BatSet.empty) in
        print_endline ("move_b " ^ string_of_set move_b);

        let eps_closure_a = BatSet.fold (fun s set -> let n_set = get_eps_closure nfa s set in BatSet.union n_set set) move_a move_a in
        print_endline ("eps_a " ^ string_of_set eps_closure_a);
        let eps_closure_b = BatSet.fold (fun s set -> let n_set = get_eps_closure nfa s set in BatSet.union n_set set) move_b move_b in
        print_endline ("eps_b " ^ string_of_set eps_closure_b);

        let a_empty = BatSet.is_empty eps_closure_a in
        let b_empty = BatSet.is_empty eps_closure_b in

          print_endline ("set_D " ^ string_of_states set_D);
          print_endline ("set_W " ^ string_of_states set_W);

        if BatSet.is_empty state then (
          let add_edge = Dfa.add_edge (Dfa.add_edge add_final_dfa (state,A,state)) (state,B,state) in
            print_endline("is_empty?");
             (* Dfa.print add_edge;  *)
            let new_set_W = BatSet.remove state set_W in
            let new_set_W1 = my_add_eps_clousre (BatSet.mem eps_closure_a set_D) eps_closure_a new_set_W in
            let new_set_W2 = my_add_eps_clousre (BatSet.mem eps_closure_b set_D) eps_closure_b new_set_W1 in
            let new_set_D = BatSet.add eps_closure_b (BatSet.add eps_closure_a set_D) in
              print_endline ("new_set_D " ^ string_of_states new_set_D);
              print_endline ("new_set_W2 " ^ string_of_states new_set_W2);
              print_endline (" ");

            if BatSet.is_empty new_set_W2 then
              add_edge
           else
              eval_nfa nfa add_edge (fst (BatSet.pop_min new_set_W2)) new_set_D new_set_W2
        )
        else if BatSet.is_empty set_W then (print_endline ("empty set_W!"); add_final_dfa)
        else
          let new_set_W = BatSet.remove state set_W in
          let new_set_W1 = my_add_eps_clousre (BatSet.mem eps_closure_a set_D) eps_closure_a new_set_W in
          let new_set_W2 = my_add_eps_clousre (BatSet.mem eps_closure_b set_D) eps_closure_b new_set_W1 in
          let new_set_D = BatSet.add eps_closure_b (BatSet.add eps_closure_a set_D) in
          
          print_endline ("new_set_D " ^ string_of_states new_set_D);
          print_endline ("new_set_W2 " ^ string_of_states new_set_W2);
          print_endline (" ");

          match (a_empty, b_empty) with
          | (true, true) ->
            let add = Dfa.add_state add_final_dfa BatSet.empty in
            let add2 = Dfa.add_edge (Dfa.add_edge add (state, A, BatSet.empty)) (state, B, BatSet.empty) in
            let eval = eval_nfa nfa add2 eps_closure_a new_set_D new_set_W2 in
            let eval2 = eval_nfa nfa eval eps_closure_b new_set_D new_set_W2 in
              (* Dfa.print eval2; *)
              print_endline ("t,t");
              eval2
          | (true, false) ->
            let add = Dfa.add_state add_final_dfa BatSet.empty in 
            let add2 = Dfa.add_state add eps_closure_b in
            let edge1 = Dfa.add_edge add2 (state, A, BatSet.empty) in
            let edge2 = Dfa.add_edge edge1 (state, B, eps_closure_b) in
            if BatSet.mem eps_closure_a new_set_W2 then 
              let eval = eval_nfa nfa edge2 eps_closure_a new_set_D new_set_W2 in
              print_endline ("false, false");
              (* Dfa.print eval; *)
              eval
            else
              let eval = eval_nfa nfa edge2 eps_closure_b new_set_D new_set_W2 in
              print_endline ("false, false");
              (* Dfa.print eval; *)
              eval
          | (false, true) ->
            let add = Dfa.add_state add_final_dfa BatSet.empty in 
            let add2 = Dfa.add_state add eps_closure_a in
            let edge1 = Dfa.add_edge add2 (state, B, BatSet.empty) in
            let edge2 = Dfa.add_edge edge1 (state, A, eps_closure_a) in

            if BatSet.mem eps_closure_a new_set_W2 then 
              let eval = eval_nfa nfa edge2 eps_closure_a new_set_D new_set_W2 in
              print_endline ("false, false");
              (* Dfa.print eval; *)
              eval
           else if BatSet.mem eps_closure_b new_set_W2 then (
              (* print_string ("here?\n"); *)
              let eval = eval_nfa nfa edge2 eps_closure_b new_set_D new_set_W2 in
              print_endline ("false, false");
              Dfa.print eval;
              eval)
            else
              edge2 
          
          | (false, false) -> 
            let add = Dfa.add_state add_final_dfa eps_closure_a in 
            let add2 = Dfa.add_state add eps_closure_b in
            let edge1 = Dfa.add_edge add2 (state, A, eps_closure_a) in
            let edge2 = Dfa.add_edge edge1 (state, B, eps_closure_b) in

            (* Dfa.print edge2; *)

            if BatSet.mem eps_closure_a new_set_W2 then 
              let eval = eval_nfa nfa edge2 eps_closure_a new_set_D new_set_W2 in
              print_endline ("false, false");
              Dfa.print eval;
              eval
            else if BatSet.mem eps_closure_b new_set_W2 then (
              print_string ("here?\n");
              let eval = eval_nfa nfa edge2 eps_closure_b new_set_D new_set_W2 in
              print_endline ("false, false");
              Dfa.print eval;
              eval)
            else
              edge2 

 
let nfa2dfa : Nfa.t -> Dfa.t
=fun nfa -> 
  let nfa_init_state = Nfa.get_initial_state nfa in 
  let dfa_init_state = get_eps_closure nfa nfa_init_state (BatSet.singleton nfa_init_state) in
  let new_dfa = Dfa.create_new_dfa dfa_init_state in
  let result_dfa = eval_nfa nfa new_dfa dfa_init_state (BatSet.singleton dfa_init_state) (BatSet.singleton dfa_init_state) in
    print_endline ("  ");
    Dfa.print result_dfa;
    result_dfa


(* Do not modify this function *)
let regex2dfa : Regex.t -> Dfa.t
=fun regex -> 
  let nfa = regex2nfa regex in
  let dfa = nfa2dfa nfa in
    dfa

let rec get_last_state
=fun dfa state str ->
  if BatSet.is_empty state then state
  else 
    match str with
    | [] -> state
    | hd::tl -> get_last_state dfa (Dfa.get_next_state dfa state hd) tl

let run_dfa : Dfa.t -> alphabet list -> bool
=fun dfa str ->
  let last_state = get_last_state dfa (Dfa.get_initial_state dfa) str in
  let is_correct = Dfa.is_final_state dfa last_state in
    is_correct