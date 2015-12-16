(* Polymorphism with WRITE + EQUAL *)

let val bar = fn x => fn y =>
  if x = y then 
    write x
  else 
    write y
in
  let val i = 1
      val s = "hello world" 
      val b = true 
      val l = malloc 10
  in
    (bar i 2, bar s "bye world");
    (fn z => (z, bar "aa" "bb")) (bar b false)
  end
end
    
(* Result : (bool, string) *)
