let sigma ((a, b, f): int * int * (int -> int)): int =
  let sum = ref 0 in
  for i = a to b do
    sum := !sum + (f i)
  done;
  !sum
