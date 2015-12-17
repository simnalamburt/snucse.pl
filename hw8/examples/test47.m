let
 val i = fn x => x
 val k = fn x => fn y => x
 val s = fn x => fn y => fn z => (x z) (y z)
in
 (((s (k (s i))) ((s (k k)) i)) true) (fn x => (x or false))
end
