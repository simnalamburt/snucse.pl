(* Polymorphism trap (write, equal) : type check fail *)

let rec my_fun = fn x => fn y =>
  if x = y
  then write y
  else write (x + 1)
in
  my_fun true false
end

(* Result : type error *)
