(* Polymorphism trap (type scheme generalization) : type check fail *)

let val k = fn x =>
  (* This y should not be generalized, since x is in type env *)
  let val y = x in 
    (y 1, y true)
  end
in
  k (fn x => x or true)
end

(* Result : type error *)
