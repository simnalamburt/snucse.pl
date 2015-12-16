(* Polymorphism trap (type scheme generalization) : type check success *)

let val k = fn x =>
  (* This y should not be generalized, since x is in type env *)
  let val y = x in 
    (y false, y true)
  end
in
  k (fn x => x or true)
end

(* Result : (bool, bool) *)
