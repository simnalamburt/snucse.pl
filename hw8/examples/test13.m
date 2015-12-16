(* Polymorphism with recursion *)

let rec iter = fn f => fn n => fn a =>
  if n = 0 then a else (iter f (n - 1) (f a))
in
  ( iter (fn x => x + 1) 10 100, 
    iter (fn x => x or false) 10 false
  )
end

(* Result : (int, bool) *)
