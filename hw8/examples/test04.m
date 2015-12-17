(* Polymorphic toy 2 (type error) *)

let val I = fn x => x
    val fst = fn x => x.1
    val const = fn n => 10
in
  (I 1, I true);
  fst (1, true) + fst ("wrong", "2015 FALL 4190.310") - 
  const 1 + const true + const "programming language"
end

(* Result : type error *)
