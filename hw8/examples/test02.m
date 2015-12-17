(* Polymorphic toy 2 (type error) *)

let val f = fn x => x + 1 in
  (f 1, f true)
end

(* Result : type error *)
