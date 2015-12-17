(* Polymorphism with EQUAL + WRITE *)

let val f = fn x => fn y => (write y) = (write x) in
  ((f "a") "b", (f 1) 2)
end

(* Result : (bool, bool) *)

