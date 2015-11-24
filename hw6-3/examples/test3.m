(* Public testcase 3 : recursion *)

let rec fib = fn x =>          
  if x = 0 then 
    0
  else if x = 1 then
    1
  else (fib (x-1) + fib (x-2)) 
in
  write fib (9)
end

(* Output : prints out "34" *)
