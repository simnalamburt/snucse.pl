(* Polymorphism Recursive Function *)

let rec f = fn x =>
  (f x) = (f x); malloc (f x); write (f x); x
in
  let val x = "const" in f f x end
end

(* Result : string *)
