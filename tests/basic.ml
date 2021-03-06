#use ".ocamlinit";;

(*
 * Homework 1
 *)
(* Exercise 1 *)
let l0 = []
let l1 = [5; 3; 1]
let l2 = [4; 2; 0]
let l3 = [10; 7; 6]
let l4 = [11; 9; 8]
;;

assert ([10; 10; 4; 3; 2; 1] = (merge ([10; 4; 2], [10; 3; 1])));
assert ([10; 3; 2; -2; -3; -10] = (merge ([3; -2; -10], [10; 2; -3])));
assert (merge (l0, l0) = l0);
assert (merge (l1, l0) = l1);
assert (merge (l0, l2) = l2);
assert (merge (l1, l2) = [5; 4; 3; 2; 1; 0]);
assert (merge (l2, l1) = [5; 4; 3; 2; 1; 0]);
assert (merge (l3, l4) = [11; 10; 9; 8; 7; 6]);
assert (merge (l4, l3) = [11; 10; 9; 8; 7; 6]);
assert (merge (l1, l4) = [11; 9; 8; 5; 3; 1]);
assert (merge (merge (l1, l2), l4) = [11; 9; 8; 5; 4; 3; 2; 1; 0]);
assert (merge (merge (l1, l2), merge (l4, l3)) = [11; 10; 9; 8; 7; 6; 5; 4; 3; 2; 1; 0]);

assert (merge ([], [5;4;3;2;1]) = [5;4;3;2;1]);
assert (merge ([10;8;6;4;2], []) = [10;8;6;4;2]);
assert (merge ([3;2;1], [6;5;4]) = [6;5;4;3;2;1]);
assert (merge ([5;3;1], [4;2]) = [5;4;3;2;1]);
assert (merge ([10;2;1], [10;4;3]) = [10;10;4;3;2;1]);
;;

(* Exercise 2 *)
let foo = fun n -> n * 2
let bar = fun n -> n * n
;;

assert (385 = sigma (1, 10, fun x -> x*x));
assert (0 = sigma (3, 1, fun x -> x*x));
assert (27 = sigma (3, 3, fun x -> x*x*x));
assert (385 = sigma (-10, -1, fun x -> x*x));
assert (sigma (3, 1, foo) = 0);
assert (sigma (4, 2, bar) = 0);
assert (sigma (8, 8, foo) = 16);
assert (sigma (3, 4, bar) = 25);
assert (sigma (1, 10, foo) = 110);
assert (sigma (1, 10, bar) = 5 * 7 * 11);
assert (sigma (5, 10, foo) = 90);
assert (sigma (1, 100, foo) = 10100);

assert (sigma (10, 10, fun x -> x) = 10);
assert (sigma (11, 10, fun x -> x) = 0);
assert (sigma (10, 5, fun x -> x) = 0);
assert (sigma (1, 10, fun x -> if x mod 2 = 0 then 1 else 0 ) = 5);
assert (sigma (1, 10, fun x -> x * x) = 385);
;;

(* Exercise 3 *)
let f1 = fun x -> 2 + x
let f2 = fun x -> fun y -> x * y
;;

assert (20 = (iter (10, (fun x->x+2))) 0);
assert (100 = (iter (100, (fun x->x+1))) 0);
assert (0 = (iter (10, (fun x->x))) 0);
assert ((iter (10, f1)) 3 = 23);
assert ((iter (2, f1)) 121 = f1 (f1 121));
assert ((iter (3, f1)) 177 = f1 (f1 (f1 177)));
assert ((iter (4, f2 1)) 44 = 44);
assert (((iter (2, f2 7)) 4 = f2 7 (f2 7 4)));

assert (iter(10, fun x -> x + 3) 100 = 130);
assert (iter(0, fun x -> x + 3) 200 = 200);
assert (iter (3, List.tl) [1;2;3;4;5;6] = [4;5;6]);
assert (iter (4, (fun s -> s ^ s)) "a" = "aaaaaaaaaaaaaaaa");
assert (iter (5, fun (x,y,z) -> (z, x, y)) (1,2,3) = (2, 3, 1));
;;

(* Exercise 4 *)
let f1 = TRUE
let f2 = FALSE
let f3 = NOT f1
let f4 = ANDALSO (NOT f2, ANDALSO (f3, f1))
let f5 = ORELSE (ORELSE (f3, f1), f4)
let f6 = IMPLY (f4, f5)
let f7 = IMPLY (f5, ORELSE (f4, FALSE))
let f8 = ORELSE (IMPLY (NOT f6, f2), ANDALSO (ORELSE (f3, NOT f4), NOT f7))
let f9 = LESS (NUM 1, NUM 2)
let fa = LESS (PLUS (NUM 1, NUM 2), MINUS (NUM 0, NUM 121))
let fb =
  LESS
    (MINUS
      (PLUS (NUM 5, MINUS (NUM 1, NUM 21)),
       MINUS (NUM 0, NUM 100)), NUM 2)
;;

assert (true = eval TRUE);
assert (false = eval FALSE);
assert (false = eval (NOT TRUE));
assert (true = eval (NOT FALSE));
assert (true = eval (ANDALSO (TRUE, TRUE)));
assert (false = eval (ANDALSO (TRUE, FALSE)));
assert (false = eval (ANDALSO (FALSE, TRUE)));
assert (false = eval (ANDALSO (FALSE, FALSE)));
assert (true = eval (ORELSE (TRUE, TRUE)));
assert (true = eval (ORELSE (TRUE, FALSE)));
assert (true = eval (ORELSE (FALSE, TRUE)));
assert (false = eval (ORELSE (FALSE, FALSE)));
assert (false = eval (IMPLY (TRUE, FALSE)));
assert (true = eval (IMPLY (TRUE, TRUE)));
assert (true = eval (IMPLY (FALSE, TRUE)));
assert (true = eval (IMPLY (FALSE, FALSE)));
assert (true = eval (LESS (NUM 3, NUM 5)));
assert (false = eval (LESS (NUM 3, NUM 3)));
assert (false = eval (LESS (NUM 3, NUM 1)));
assert (false = eval (LESS (PLUS (NUM 3, NUM 4), MINUS (NUM 5, NUM 1))));
assert (true = eval (LESS (PLUS (NUM 10, NUM 12), MINUS (NUM 10, NUM (-13)))));
assert (eval f1 = true);
assert (eval f2 = false);
assert (eval f3 = false);
assert (eval f4 = false);
assert (eval f5 = true);
assert (eval f6 = true);
assert (eval f7 = false);
assert (eval f8 = true);
assert (eval f9 = true);
assert (eval fa = false);
assert (eval fb = false);

assert (eval (IMPLY(FALSE, FALSE)) = true);
assert (eval (ANDALSO (ORELSE(TRUE, FALSE), NOT (IMPLY(TRUE, FALSE)))) = true);
assert ((eval (ANDALSO (TRUE, TRUE))) && (not (eval (ANDALSO (TRUE, FALSE)))) && (not (eval (ANDALSO (FALSE, TRUE)))) && (not (eval (ANDALSO (FALSE, FALSE)))) = true);
assert ((eval (ORELSE (TRUE, TRUE))) && (eval (ORELSE (TRUE, FALSE))) && (eval (ORELSE (FALSE, TRUE))) && (not (eval (ORELSE (FALSE, FALSE)))) = true);
assert ((eval (IMPLY (TRUE, TRUE))) && (not (eval (IMPLY (TRUE, FALSE)))) && (eval (IMPLY (FALSE, TRUE))) && (eval (IMPLY (FALSE, FALSE))) = true);
assert (eval (LESS (NUM 3, NUM 5)) = true);
assert (eval (LESS (PLUS (NUM 3, NUM 5), PLUS (NUM 1, NUM 2))) = false);
assert (eval (LESS (MINUS (NUM 3, NUM 5), MINUS (NUM 1, NUM 2))) = true);
assert (eval (ORELSE (LESS (PLUS (MINUS (NUM 3, NUM 2), NUM 9), NUM 10), FALSE)) = false);
assert (eval (IMPLY(LESS (NUM 1, NUM 0), ANDALSO(TRUE, ORELSE(NOT TRUE, LESS(NUM 2, NUM 1))))) = true);
;;

(* Exercise 5 *)
let one = SUCC ZERO
let three = SUCC (SUCC (SUCC ZERO))
let four = SUCC three
let rec nat_to_int (input: nat): int =
  match input with
  | ZERO -> 0
  | SUCC(i1) -> 1 + (nat_to_int i1)
let rec int_to_nat (input: int): nat =
  match input with
  | 0 -> ZERO
  | _ -> SUCC (int_to_nat (input - 1))
;;

assert (7 = nat_to_int (natadd (three, four)));
assert (0 = nat_to_int (natadd (ZERO, ZERO)));
assert (3 = nat_to_int (natadd (ZERO, three)));
assert (4 = nat_to_int (natadd (four, ZERO)));

assert (12 = nat_to_int (natmul (three, four)));
assert (0 = nat_to_int (natmul (ZERO, three)));
assert (0 = nat_to_int (natmul (four, ZERO)));
assert (0 = nat_to_int (natmul (ZERO, ZERO)));
assert (3 = nat_to_int (natmul (SUCC ZERO, three)));
assert (4 = nat_to_int (natmul (four, SUCC ZERO)));

assert (nat_to_int (natadd (int_to_nat 0, int_to_nat 0)) = 0);
assert (nat_to_int (natadd (int_to_nat 2, int_to_nat 0)) = 2);
assert (nat_to_int (natadd (int_to_nat 0, int_to_nat 3)) = 3);
assert (nat_to_int (natadd (int_to_nat 1, int_to_nat 5)) = 6);
assert (nat_to_int (natadd (int_to_nat 3, int_to_nat 3)) = 6);
assert (nat_to_int (natadd (int_to_nat 12, int_to_nat 7)) = 19);
assert (nat_to_int (natadd (int_to_nat 34, int_to_nat 19)) = 53);

assert (nat_to_int (natmul (int_to_nat 0, int_to_nat 0)) = 0);
assert (nat_to_int (natmul (int_to_nat 2, int_to_nat 0)) = 0);
assert (nat_to_int (natmul (int_to_nat 0, int_to_nat 3)) = 0);
assert (nat_to_int (natmul (int_to_nat 1, int_to_nat 5)) = 5);
assert (nat_to_int (natmul (int_to_nat 3, int_to_nat 3)) = 9);
assert (nat_to_int (natmul (int_to_nat 11, int_to_nat 7)) = 77);
assert (nat_to_int (natmul (int_to_nat 8, int_to_nat 12)) = 96);

assert (natadd (ZERO, ZERO) = ZERO);
assert (natadd (ZERO, (SUCC (SUCC ZERO))) = (SUCC (SUCC ZERO)));
assert (natadd ((SUCC (SUCC ZERO)), (SUCC (SUCC (SUCC ZERO)))) = (SUCC (SUCC (SUCC (SUCC (SUCC ZERO))))));
assert (natmul (ZERO, (SUCC (SUCC ZERO))) = ZERO);
assert (natmul ((SUCC (SUCC ZERO)), (SUCC (SUCC (SUCC ZERO)))) = (SUCC (SUCC (SUCC (SUCC (SUCC (SUCC ZERO)))))));
;;

(*
 * Homework 2
 *)
(* Exercise 1 *)
let zero = ZERO NIL
let one = MONE (ONE (ZERO NIL))
let four = ZERO (ZERO (MONE (ONE NIL)))
let m_five = ONE (MONE (ONE (MONE NIL)))
let m_one = (ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(ONE(MONE NIL))))))))))))))))))))))))))))))
;;

assert ( 0 = crazy2val zero);
assert ( 1 = crazy2val one);
assert ( 4 = crazy2val four);
assert (-5 = crazy2val m_five);
assert (-1 = crazy2val m_one);
;;

(* Exercise 3 *)
assert (checkMetro (AREA("a", STATION "a") ));
assert (checkMetro (AREA("a", AREA("a", STATION "a")) ));
assert (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "b"))) ));
assert (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "a"))) ));
assert (checkMetro ( CONNECT (AREA ("a", STATION "a"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "a")))) ));

assert (not (checkMetro (AREA("a", STATION "b") )));
assert (not (checkMetro (AREA("a", CONNECT(STATION "a", AREA("b", STATION "c"))) )));
assert (not (checkMetro (AREA("a", AREA("b", CONNECT(STATION "a", STATION "c"))) )));
assert (not (checkMetro ( CONNECT (AREA ("c", STATION "c"), AREA ("b", AREA("a", CONNECT(STATION "b", STATION "c")))) )));
;;

(* Exercise 4 *)
let empty = LOC (NODE [], TOP)
let loc = LOC (LEAF "*", HAND ([LEAF "c"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "d"]))
let loc1 = goLeft loc
let loc2 = goUp loc
let loc3 = goRight loc
let loc4 = goLeft (goLeft loc2)
let loc5 = goRight (goDown loc4)
;;

assert (try goLeft empty = empty with NOMOVE _ -> true | _ -> false);
assert (try goRight empty = empty with NOMOVE _ -> true | _ -> false);
assert (try goUp empty = empty with NOMOVE _ -> true | _ -> false);
assert (try goDown empty = empty with NOMOVE _ -> true | _ -> false);

assert (try goDown loc = empty with NOMOVE _ -> true | _ -> false);

assert (loc1 = LOC (LEAF "c", HAND ([], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [LEAF "*"; LEAF "d"])));
assert (try goLeft loc1 = empty with NOMOVE _ -> true | _ -> false);
assert (goRight loc1 = loc);
assert (try goDown loc1 = empty with NOMOVE _ -> true | _ -> false);

assert (loc2 = LOC (NODE [LEAF "c"; LEAF "*"; LEAF "d"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [])));
assert (goLeft loc2 = LOC (LEAF "+", HAND ([NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, [NODE [LEAF "c"; LEAF "*"; LEAF "d"]])));
assert (goLeft (goLeft loc2) = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]])));
assert (try goLeft (goLeft (goLeft loc2)) = empty with NOMOVE _ -> true | _ -> false);
assert (try goRight loc2 = empty with NOMOVE _ -> true | _ -> false);
assert (goUp loc1 = loc2);
assert (goUp loc2 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"]; LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]], TOP));
assert (try goLeft (goUp loc2) = empty with NOMOVE _ -> true | _ -> false);
assert (try goRight (goUp loc2) = empty with NOMOVE _ -> true | _ -> false);
assert (try goUp (goUp loc2) = empty with NOMOVE _ -> true | _ -> false);
assert (goDown loc2 = loc1);

assert (loc3 = LOC (LEAF "d", HAND ([LEAF "*"; LEAF "c"], HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []), [])));
assert (goLeft loc3 = loc);
assert (try goRight loc3 = empty with NOMOVE _ -> true | _ -> false);
assert (goUp loc3 = loc2);
assert (try goDown loc3 = empty with NOMOVE _ -> true | _ -> false);

assert (loc4 = LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]])));
assert (loc5 = LOC (LEAF "*", HAND ([LEAF "a"], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]), [LEAF "b"])));
;;

let loc1 = LOC (NODE [NODE [LEAF "a"; LEAF "*"; LEAF "b"];
                      LEAF "+";
                      NODE [LEAF "c"; LEAF "*"; LEAF "d"]],
                TOP)

let (|>) g f = f g
;;

assert( loc1 |> goDown =
  LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"],
   HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]))
);

assert( loc1 |> goDown |> goDown =
  LOC (LEAF "a",
   HAND ([], HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]),
    [LEAF "*"; LEAF "b"]))
);

assert( loc1 |> goDown |> goUp |> goDown =
  LOC (NODE [LEAF "a"; LEAF "*"; LEAF "b"],
   HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]))
);

assert( loc1 |> goDown |> goDown |> goRight =
  LOC (LEAF "*",
   HAND ([LEAF "a"],
    HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]),
    [LEAF "b"]))
);

assert( loc1 |> goDown |> goDown |> goRight |> goLeft |> goRight |> goRight =
  LOC (LEAF "b",
   HAND ([LEAF "*"; LEAF "a"],
    HAND ([], TOP, [LEAF "+"; NODE [LEAF "c"; LEAF "*"; LEAF "d"]]),
    []))
);

assert( loc1 |> goDown |> goRight |> goRight |> goDown |> goRight =
  LOC (LEAF "*",
   HAND ([LEAF "c"],
    HAND ([LEAF "+"; NODE [LEAF "a"; LEAF "*"; LEAF "b"]], TOP, []),
    [LEAF "d"]))
);

assert (try (loc1 |> goUp |> ignore); false with NOMOVE _ -> true);
;;

(* Exercise 5 *)
let n1 = INT 3
let n2 = REAL 1.2
let n3 = INT (-2)
let n4 = REAL 0.8
let x1 = ADD (X, INT 1)
let x2 = MUL (X, (MUL (INT 2, X)))
let x3 = SUB (MUL (X, X), INT 1)
let s1 = SIGMA (INT 0, INT 1, X)
let s2 = SIGMA (INT 0, X, MUL (MUL (X, X), INT 3))
let s3 = SIGMA (s1, INT 10, s2)

let check (sigma: float) (a: float) (b: float): bool = a -. b < sigma && b -. a < sigma
let check_tight = check 0.00001
let check_loose = check 0.005
;;

(* Arithmatic *)
assert (check_tight (galculator n1) 3.0);
assert (check_tight (galculator n2) 1.2);
assert (check_tight (galculator n3) (-2.0));
assert (check_tight (galculator n4) 0.8);
assert (check_tight (galculator (ADD (n1, n2))) 4.2);
assert (check_tight (galculator (ADD (ADD (n1, n2), n3))) 2.2);
assert (check_tight (galculator (ADD (ADD (n1, n2), n4))) 5.0);
assert (check_tight (galculator (SUB (n1, n2))) 1.8);
assert (check_tight (galculator (SUB (n4, n3))) 2.8);
assert (check_tight (galculator (SUB (SUB (n4, n3), n3))) 4.8);
assert (check_tight (galculator (MUL (n1, n2))) 3.6);
assert (check_tight (galculator (MUL (ADD (n3, n4), n2))) (-1.44));
assert (check_tight (galculator (MUL (n1, (SUB (INT 0, n2))))) (-3.6));
assert (check_tight (galculator (DIV (n1, n2))) 2.5);
assert (check_tight (galculator (DIV (n4, n3))) (-0.4));
assert (try check_tight (galculator X) 123.0 with FreeVariable -> true | _ -> false);

(* Sigma *)
assert (galculator (SIGMA (INT 10, INT 1, X)) = 0.0);
assert (check_tight (galculator (SIGMA (INT 1, INT 10, REAL 0.5))) 5.0);
assert (check_tight (galculator (SIGMA (INT 1, INT 10, X))) 55.0);
assert (check_tight (galculator (SIGMA (REAL 1.0, INT 100, x1))) 5150.0);
assert (check_tight (galculator (SIGMA (REAL 1.0, REAL 10.1, x2))) 770.0);
assert (check_tight (galculator (SIGMA (INT 4, INT 12, MUL ((SUB (X, REAL 1.0)), x1)))) 627.0);
assert (check_tight (galculator (SIGMA (INT 4, INT 12, x3))) 627.0);
assert (check_tight (galculator s3) 3630.0);
assert (
  check_tight (galculator
  (SIGMA
    (SUB (INT 3, REAL 1.0),
    SIGMA (INT 1, INT 3, X),
    SIGMA (X, ADD (X, X), SUB (MUL (INT 2, MUL (X, X)), MUL (REAL 3.0, X))))))
  2015.0);
assert (
  check_tight (galculator
  (SIGMA (SIGMA (INT 2, INT 1, X), INT 10,
    (SIGMA (SIGMA (INT (-1), INT 1, X), X,
      (SIGMA (INT 0, X, MUL (X, X))))))))
  3289.0);
assert (try check_tight (galculator (SIGMA (INT 0, X, X))) 0.0 with FreeVariable -> true | _ -> false);

(* Integral *)
assert (check_loose (galculator (INTEGRAL (INT 2, REAL 2.05, ADD (X, X)))) 0.0);
assert (check_loose (galculator (INTEGRAL (REAL (-2.0), REAL (-2.05), DIV (X, X)))) 0.0);
assert (check_loose (galculator (INTEGRAL (INT 0, INT 2, REAL 0.5))) 1.0);
assert (check_loose (galculator (INTEGRAL (INT 2, INT 0, REAL (-1.0)))) 2.0);
assert (check_loose (galculator (INTEGRAL (INT 0, INT 2, MUL (X, INT 2)))) 3.8);
assert (check_loose (galculator (INTEGRAL (REAL 1.55, REAL 1.37, X))) (-0.137));
assert (check_loose (galculator (INTEGRAL (INT 2, INT 0, MUL (X, X)))) (-2.47));
assert (check_loose (galculator (INTEGRAL (REAL 0.1, INT 1, DIV (INT 100, X)))) 282.896);
assert (check_loose (galculator (INTEGRAL (REAL 10.0, REAL 1.0, SUB(MUL(X, X), INT 1)))) (-319.065));
assert (check_loose (galculator (INTEGRAL (INT 1, INT (-1), MUL (MUL (X, X), X)))) 0.1);
assert (check_loose (galculator (INTEGRAL (INT 1, INT 0, MUL (INT 3, MUL (X, X))))) (-0.855));
assert (try check_loose (galculator (INTEGRAL (INT 0, MUL (INT 1, X), X))) 0.0 with FreeVariable -> true | _ -> false);
assert (
  try check_loose
    (galculator
      (INTEGRAL
        (ADD (X, REAL 1.0),
        SIGMA (REAL 1.0, REAL 10.0, SUB (MUL(X, X), INT 1)),
        SUB(MUL(X, X), X))))
    0.0 with FreeVariable -> true | _ -> false);
assert (
  try check_loose
    (galculator
      (SIGMA
        (INTEGRAL (INT 0, MUL (INT 1, INT 0), X),
        INTEGRAL (SUB (INT 1, MUL (INT 2, X)), INT 30, REAL 10.5),
        MUL (X, X))))
    0.0 with FreeVariable -> true | _ -> false);

(* etc *)
assert (try check_tight (galculator X) nan with _ -> true);
assert (try check_tight (galculator (INTEGRAL (ADD (X, REAL 1.), SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X))))
  nan with _ -> true);

assert (check_tight (galculator (SIGMA (INT 1, INT 10, SUB(MUL(X, X), INT 1)))) 375.0);
assert (check_tight (galculator (SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)))) 375.0);
assert (check_loose (galculator (INTEGRAL (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)))) 319.064999999);
assert (check_loose (galculator (INTEGRAL (REAL 10., REAL 1., SUB(MUL(X, X), INT 1)))) (-319.06499999));
(*
assert (check_loose (galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), INT 1)))) 17556346.1239999793);
assert (check_loose (galculator (INTEGRAL (REAL 10., SIGMA (REAL 1., REAL 10., SUB(MUL(X, X), INT 1)), SUB(MUL(X, X), X)))) 17486504.2639999);
*)

(* Exercise 6 *)
module ValidIntListQ = (IntListQ : Queue)

let q1 = IntListQ.emptyQ
let q2 = IntListQ.enQ (q1, [1])
let q3 = IntListQ.enQ (q2, [2])
let q4 = IntListQ.enQ (q3, [3])
let x1, q5 = IntListQ.deQ q4
let q6 = IntListQ.enQ (q5, [4])
let q7 = IntListQ.enQ (q6, [5])
let q8 = IntListQ.enQ (q7, [6])
let x2, q9 = IntListQ.deQ q8
let x3, qa = IntListQ.deQ q9
let x4, qb = IntListQ.deQ qa
let x5, qc = IntListQ.deQ qb
let x6, qd = IntListQ.deQ qc
;;

assert (fst (IntListQ.deQ q2) = [1]);
assert (fst (IntListQ.deQ q5) = [2]);
assert (fst (IntListQ.deQ (snd (IntListQ.deQ q5))) = [3]);
assert (x1 = [1]);
assert (x2 = [2]);
assert (x3 = [3]);
assert (x4 = [4]);
assert (x5 = [5]);
assert (x6 = [6]);
assert (qd = q1);
assert (try IntListQ.deQ qd = ([], q1) with IntListQ.EMPTY_Q -> true | _ -> false);
;;

let q1 = IntListQ.emptyQ
let q2 = IntListQ.enQ(IntListQ.enQ(IntListQ.enQ(q1, [1]), [2; 3]), [4; 5; 6])
let e1, q3 = IntListQ.deQ q2
;;

assert (q1 = ([], []));
assert (q2 = ([[4; 5; 6]; [2; 3]; [1]], []));
assert (e1 = [1]);
assert (q3 = ([], [[2; 3]; [4; 5; 6]]));
;;


(*
 * Homework 5
 *)


(*
 * Homework 6
 *)
