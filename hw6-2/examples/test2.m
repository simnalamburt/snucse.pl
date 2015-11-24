(* Public testcase 2 : Function application *)

((fn x => (fn y => x + y)) 5) ((fn z => z) 3)

(* Output : 8 *)
