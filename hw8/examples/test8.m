(* Polymorphic swap (type error) *)

let val swap = (fn order_pair => 
  if (order_pair.1) (order_pair.2) then
    (order_pair.2)
  else 
    (order_pair.2.2, order_pair.2.1))
in
  ( swap (fn pair => pair.1 + 1 = pair.2, (1, 2)),
    swap (fn pair => pair.1 or pair.2, (4, 3))
  )
end

(* Result : type error *)
