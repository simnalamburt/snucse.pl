let rec foo = fn f => fn x => fn y =>
     f x y
in
 foo (fn x => fn y => (x + y)) 1 2;
 foo (fn x => fn y => (x = true)) true 2
end

(* bool *)
