let
 val f1 = fn x => x
 val foo = fn f => fn x => f f1 x
in
 f1 true;
 foo f1 "sad"
end
(* string *)
