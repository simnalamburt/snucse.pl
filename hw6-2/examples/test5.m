(* Public testcase 5 : fibonaci with recursion. *)

(rec fib x => (ifzero x then 0 else (ifzero (x + (-1)) then 1 else (fib (x + (-2)) + fib (x + (-1)))))) 8

(* Output : 21 *)
