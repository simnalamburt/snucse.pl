(* Polymorphic toy with imperatives *)

let val f = fn x => malloc x in 
  let val a = f 10 
      val b = f "pl"
      val c = f true 
  in
    a := !a + 1; 
    (b := "type checker", c := !c or false)
  end
end

(* Result : (string, bool) *)
