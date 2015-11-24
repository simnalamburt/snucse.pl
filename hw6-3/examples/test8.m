(* Public testcase 8 : type error *)

let val x = malloc true in
  write (1 + !x)
end

(* Output : type error *)
