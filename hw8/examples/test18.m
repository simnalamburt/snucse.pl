(* Polymorphism with EQUAL (type error) *)

let val foo = fn x => fn y =>
  if x = y then x else y
in
  let val i = 1
      val s = "hello world" 
      val b = true 
      val l = malloc 10
  in
    (
      (foo i 2, foo s "bye world"), 
      (foo (fn x => x + 1) (fn y => y - 1), foo l (malloc 20))
    )
  end
end

(* Reseult : type error *)
