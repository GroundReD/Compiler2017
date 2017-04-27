open Regex
open Hw1

let testcases : (Regex.t * alphabet list) list = 
  [ 
    (* (Empty, []); *)
    (* (Epsilon, []); *)
    (* (Alpha A, [A]); *)
    (* (Alpha A, [B]); *)
    (* (OR (Alpha A, Alpha B), [B]); *)
    (* (CONCAT (STAR (Alpha A), Alpha B), [B]); *)
    (* (CONCAT (STAR (Alpha A), Alpha B), [A;B]); *)
    (* (CONCAT (STAR (Alpha A), Alpha B), [A;A;B]); *)
    (* (CONCAT (STAR (Alpha A), Alpha B), [A;B;B]); *)
    (* (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [B]); *)
    (* (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;B]); *)
    (* (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [B;B;B]); *)
    (* (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;A;A;B;B;B]); *)
    (* (CONCAT (CONCAT (STAR (CONCAT (Alpha A, Alpha A)), STAR (CONCAT (Alpha B, Alpha B))), Alpha B), [A;A;A;B;B;B]); *)
    (* (STAR Empty, []); *)
    (* (STAR (CONCAT (Alpha A, Empty)), []); *)
    (* (CONCAT (STAR Empty, CONCAT (STAR (CONCAT (Alpha A, STAR (CONCAT (Alpha A, Empty)))), STAR (Alpha B))), [A;B]); *)
    (* (CONCAT (CONCAT (Alpha A, Alpha B), CONCAT (STAR (OR (Alpha A, Alpha B)), Empty)), [A;B;A;A]); *)
    (* (STAR (CONCAT (CONCAT (Alpha B, Alpha A), Alpha A)), [B;A;A;B]); *)
    (* (CONCAT (Alpha A, STAR (OR (Alpha A, Alpha B))), [A;A;B;A]); *)
    (* (STAR (CONCAT (CONCAT (Alpha A, STAR (OR (Alpha B, Alpha A))), Alpha B)), [A;A;B;A;B;A]); *)
    (* (CONCAT (OR (Alpha A, STAR (Alpha B)), CONCAT (Alpha A, STAR (CONCAT (Alpha A, Alpha B)))), [B;B;A;A;B]); *)
    (STAR (STAR (STAR (OR (Alpha B, CONCAT (STAR (CONCAT (Alpha A, Empty)), CONCAT (Alpha A, CONCAT (Alpha A, CONCAT (Alpha A, Alpha B)))))))), [A;A;A;B;B;B;A]);
    (CONCAT (STAR (CONCAT (Alpha A, Alpha B)), OR (STAR (STAR (OR (Alpha B, Alpha A))), STAR (CONCAT (Alpha A, Alpha B)))), [A;B;A;B;B;B;A;A;B;A;B])
  ]

let match_regex : Regex.t -> alphabet list -> bool
=fun regex input -> Hw1.run_dfa (Hw1.regex2dfa regex) input

(* run testcases *)
let _ = 
  List.iter (fun (regex, str) -> 
    prerr_endline (string_of_bool (match_regex regex str)) 
  ) testcases