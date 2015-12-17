(* Polymorphism with EQUAL + WRITE *)

let rec f = fn x => (write x) = (write x) in
  (f "a", f 1)
end

(* Result : (bool, bool) *)
