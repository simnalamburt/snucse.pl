(* Polymorphic toy (seq) *)

let val f = fn x =>
  (read; x)
in
  f 1
end

(* Result : int *)
