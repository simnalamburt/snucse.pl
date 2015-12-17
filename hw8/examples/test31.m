(* Polymorphism Recursive Function *)

let rec f = fn x =>
  f (malloc x); f (write "1"); x
in
  ((f 1) = (f f 1), f f "1")
end

(* Result : (bool, string) *)
