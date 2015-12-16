(* Self applicative function *)

let val f = fn x => x in
  f ((f f) 1, f "abc")
end

(* Result : (int, string) *)
