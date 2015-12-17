(* Polymorphism trap WRITE + EQUAL *)

let val foo = fn x => fn y =>
  malloc ((write x) = (write y))
in
  (foo true false, (foo 4 5, foo (malloc 4) (malloc 5)))
end

(* Result : type error *)
