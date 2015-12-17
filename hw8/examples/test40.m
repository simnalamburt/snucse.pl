(* Polymorphism trap (unbound variable) *)

let val y = fn x => y
in
  y 1
end

(* Reseult : type error *)
