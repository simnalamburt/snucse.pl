let
 val f1 = fn x => x.1
 val f2 = fn f => fn op => fn x => op (f x)
in
 f2 f1 (fn x => (x + 1)) (1, true);
 f2 f1 (fn x => (x = "asd")) ("zxc", 1)
end
