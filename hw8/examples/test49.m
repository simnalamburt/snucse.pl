let
 val f = fn x => malloc x
 val a = f 10
 val b = f "pl"
 val c = f true
in
 a := (!a + 1);
 b := "hw7";
 c := (!c or false)
end
