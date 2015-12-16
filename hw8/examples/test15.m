(* Polymorphism with WRITE *)

let val print = fn x => 
  (write x; true)
in
  (print 1, (print "hello world", print true))
end

(* Reseult : (bool, (bool, bool)) *)
