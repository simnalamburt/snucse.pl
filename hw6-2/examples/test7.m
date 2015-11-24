(* Public testcase 7 : Recursion and high order function *)

(fn f => f (f 4))
(rec sum x => (ifzero x then 0 else (sum (x + (-1)) + x)))

(* Output : 55 *)
