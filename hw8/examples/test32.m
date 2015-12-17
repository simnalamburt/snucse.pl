(* Polymorphism Recursive Function : type check faild *)

let rec f = fn x =>
  x = x; malloc x; write x; x
in
  let val x = malloc 1 in f x end
end

(* Result : type error *)
