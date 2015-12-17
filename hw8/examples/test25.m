(* Polymorphism with WRITE *)

let val print = fn x => 
  write x
in
  (print 1, (print "hello world", print true))
end

(* Reseult : (int, (string, bool)) *)
