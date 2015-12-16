(* Polymorphism trap (malloc) : type check fail *)

let val my_malloc = fn x => malloc x in
  let val f = (write "calling my_malloc"; my_malloc (fn x => x)) in 
    f := (fn x => x + 1); write ((!f) 10); (!f) true
  end
end

(* Result : type error *)
