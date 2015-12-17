(* Polymorphic toy 1 *)

let val f = fn x => x in
  (f 1, f true)
end

(* Result : (int, bool) *)
