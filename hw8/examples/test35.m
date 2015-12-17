(* Polymorphism with WRITE *)
let rec f = fn x=> 
  let rec g = fn x => 
    f x 
  in 
    g x 
  end 
in 
  f; 
  (f true) + 1; 
  (f 1) and false 
end 

(* Reseult : bool *)
