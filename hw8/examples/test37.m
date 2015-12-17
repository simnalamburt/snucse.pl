(* Polymorphism with WRITE *)

let val f = fn x => fn y => 
  write (x = y) 
in 
  (f (malloc 1)) (malloc 3) 
end 

(* Reseult : bool *)
