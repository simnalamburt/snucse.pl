(* Public testcase 9 : type error *)

let val x = malloc (1, 2) in
  write !x
end

(* Output : type error *)
