(* Self applicative function *)

let val f = fn x => x + 1 in
  ((f f) 1, f 2)
end

(* Result : type error *)
