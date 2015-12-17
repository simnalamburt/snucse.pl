(* Polymorphism with WRITE *)

let val print = fn x => 
  (write x; malloc x)
in
  (!(print 1), let val tmp = !(print false) in !(tmp) end)
end

(* Reseult : type error *)
