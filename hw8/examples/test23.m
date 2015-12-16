(* Polymorphism trap (malloc) : type check success *)

let rec f = fn x => malloc (fn x => x) in (* This does not invoke malloc *)
  write ((!(f 10)) true); write ((!(f true)) "hello")
end

(* Result : string *)
