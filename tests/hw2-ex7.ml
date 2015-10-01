#use "hw2/ex7.ml"

(* Homework 2 - Exercise 7 *)
module ValidateZexpr = (Zexpr : ZEXPR)
open Zexpr

let test (expr: expr) (msg: string) =
  print_value (eval (emptyEnv, expr));
  print_endline ("\n" ^ msg ^ "\n")
let ex_test (expr: expr) =
  assert (try test expr "Error"; false with | _ -> true)

let xpx = PLUS (VAR "x", VAR "x")
let xmx = MULT (VAR "x", VAR "x")
let lyxx = LET ("y", xpx, MULT (NUM (-1), VAR "y"))
let zyyx = LET ("z", xpx,
  LET ("y", MAX [NUM (-3); NUM (-2); NUM (-1)],
    LET ("y", PLUS (VAR "y", VAR "z"),
      LET ("x", xmx, DIVIDE (MULT (VAR "x", VAR "y"), NUM 2)))))
;;

test (NUM 7) "7";
test (LET ("x", NUM 12, VAR "x")) "12";
test (LET ("x", NUM 10, MINUS (NUM 8, VAR "x"))) "-2";
test (LET ("x", NUM 10, PLUS (NUM (-8), VAR "x"))) "2";
test (LET ("x", NUM 2, DIVIDE (NUM 8, VAR "x"))) "4";

test (MAX []) "0";
test (MAX [NUM min_int]) (string_of_int min_int);
test (LET ("x", NUM (-1), MAX [VAR "x"; NUM (-2)])) "-1";
test (LET ("x", NUM 2, MAX [NUM 1; NUM (-3); MAX []; lyxx; xpx])) "4";

test (LET ("y", NUM 1, PLUS (LET ("y", NUM 2, PLUS (VAR "y", VAR "y")), VAR "y"))) "5";
test (LET ("x", NUM 1, PLUS (LET ("y", NUM 2, PLUS (VAR "x", VAR "y")), VAR "x"))) "4";
test (LET ("x", NUM 4, MULT (LET ("x", xpx, xpx), LET ("x", NUM 2, LET ("x", xpx, xpx))))) "128";
test (LET ("x", NUM 2, MULT (LET ("x", NUM 1, MAX [NUM 0; VAR "x"; NUM 3]), VAR "x"))) "6";
test (LET ("x", MAX [NUM 3; NUM 8; NUM 5; NUM 2; NUM 8; NUM 7; NUM 6], LET ("y", PLUS (xmx, xpx), PLUS (LET ("x", NUM 1, lyxx), zyyx)))) "478";

ex_test (LET ("y", NUM 2, VAR "x"));
ex_test (LET ("x", NUM 1, PLUS (LET ("y", NUM 2, PLUS (VAR "x", VAR "y")), VAR "y")));
ex_test (LET ("x", MAX [], (LET ("y", NUM 128, DIVIDE (VAR "y", VAR "x")))));
ex_test (LET ("x", MAX [], (LET ("y", NUM 0, DIVIDE (VAR "y", VAR "x")))));
;;

let xpx = Zexpr.PLUS((Zexpr.VAR "x"),(Zexpr.VAR "x"))
let e1 = Zexpr.LET("x", (Zexpr.NUM 1), (Zexpr.PLUS (Zexpr.LET("x", xpx, xpx), (Zexpr.VAR "x"))))
;;

test (Zexpr.NUM 1) "1";
test e1 "5";
test (Zexpr.MAX []) "0";
test (Zexpr.MAX [Zexpr.NUM (-1)]) "-1";
;;
