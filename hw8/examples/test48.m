let
 val c = malloc fn x =>
    x
in
 c := fn x =>
  (1 + x);
 !c true
end
