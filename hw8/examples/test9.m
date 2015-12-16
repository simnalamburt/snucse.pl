(* SKI combinators *)

let val I = fn x => x
    val K = fn x => fn y => x
    val S = fn x => fn y => fn z => (x z) (y z)
in
  S (K (S I)) (S (K K) I) 1 (fn x => x + 1)
end

(* Result : int *)
