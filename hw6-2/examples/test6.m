(* Public testcase 6 : high order function *)

(fn f => f (f 5)) (fn x => x + 10)

(* Output : 25 *)
