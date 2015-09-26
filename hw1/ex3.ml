let rec iter ((n, f): int * ('a -> 'a)): ('a -> 'a) =
  if n <= 0 then
    function x -> x
  else
    let compose f g x = f(g(x)) in
    compose f (iter ((n-1), f))
