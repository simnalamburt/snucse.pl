(* Polymorphism with ASSIGN *)

let val print = fn x =>
  (write x; malloc x)
in
  (print 3) := (malloc 4) := 5
end

(* Reseult : int *)
