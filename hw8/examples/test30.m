(* Self applicative function *)

let rec f = fn x => x.2 + 1 in
  (f (f, f) (1, 2), f (2, 3))
end

(* Result : type error *)
