(* Polymorphic toy 38 (type error) *)

let rec f = fn x =>
  (f (malloc true); if x = 0 then x else (x + (f (x - 1))))
in
  (f 5)
end

(* Result : type error *)
