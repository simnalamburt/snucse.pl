(* Public testcase 4 : high order function *)

let 
  val x = malloc 0 
  val f = fn g => g !x
  val incr = fn x => x + 1
in
  x := read; write (f incr)
end

(* Output : reads n, and output 'n + 1' *)
