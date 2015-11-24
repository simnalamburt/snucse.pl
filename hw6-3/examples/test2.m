(* Public testcase 2 : malloc, pair *)

let val x = (malloc 3, malloc 4) in 
  x.1 := read; 
  x.2 := read; 
  write (!x.1 + !x.2) 
end 

(* Output : reads n1, n2 and output 'n1 + n2' *)
