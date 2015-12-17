(* Polymorphism with recursion (type error) *)

let rec foo = fn x => fn y => fn z =>
  if z = 0
  then
    0
  else
    foo (x y) (y x) (z - 1)
in
  foo (fn x => x) (fn y => y) 3
end

(* Result : type error *)
