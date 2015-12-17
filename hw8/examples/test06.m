(* Polymorphic toy with imperatives (type error) *)

let val f = fn x => malloc x in 
  let val a = f 10 
      val b = f "pl"
      val c = f true 
  in
    a := !a + 1; 
    (c := !c + 1; b := "type checker")
  end
end

(* Result : type error *)
