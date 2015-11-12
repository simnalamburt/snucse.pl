(*
 * SNU 4190.310 Programming Languages
 * K-- to SM5 translator skeleton code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open K
open Sm5
module Translator = struct
  let rec trans (input: K.program): Sm5.command =
    let rec desugar (input: K.program): K.program =
      match input with
      | K.WHILE (econd, ebody) -> begin
        (*
         * while(econd) { ebody; }
         *
         * // ... is same with ...
         *
         * function f() {
         *   if (econd) {
         *     ebody;
         *     f()
         *   }
         * };
         * f()
         *)
        let tempname = "λwhile" in
        K.LETF (
          tempname, "γ",
            K.IF (econd,
              K.SEQ (
                ebody,
                desugar (K.CALLV (tempname, K.UNIT))
              ),
            K.UNIT),
          desugar (K.CALLV (tempname, K.UNIT))
        )
      end
      | K.FOR (i, efrom, eto, ebody) -> begin
        (*
         * for i in efrom..eto {
         *   ebody
         * }
         *
         * // ... is same with ...
         *
         * function f(i) {
         *   int temp = i;
         *   if (eto < i) {
         *     ()
         *   } else {
         *     ebody;
         *     f(temp + 1);
         *   }
         * };
         * f(efrom)
         *)
        let tempname = "λfor" in
        let tempvar = "α" in
        K.LETF (
          tempname, i,
          K.LETV (tempvar, K.VAR i,
            K.IF (K.LESS(eto, K.VAR i),
              K.UNIT,
              K.SEQ (
                ebody,
                desugar (K.CALLV (tempname, K.ADD (K.VAR tempvar, K.NUM 1)))
              )
            )
          ),
          desugar (K.CALLV (tempname, efrom))
        )
      end
      | K.CALLV (name, eparam) -> begin
        let tempname = "β" in
        K.LETV (tempname, eparam, K.CALLR(name, tempname))
      end
      | _ -> input
    in
    match (desugar input) with
    | K.NUM i -> [Sm5.PUSH (Sm5.Val (Sm5.Z i))]
    | K.TRUE -> [Sm5.PUSH (Sm5.Val (Sm5.B true))]
    | K.FALSE -> [Sm5.PUSH (Sm5.Val (Sm5.B false))]
    | K.UNIT -> [Sm5.PUSH (Sm5.Val (Sm5.Unit))]
    | K.VAR name -> [Sm5.PUSH (Sm5.Id name); Sm5.LOAD]
    | K.ADD (e1, e2) -> trans e1 @ trans e2 @ [Sm5.ADD]
    | K.SUB (e1, e2) -> trans e1 @ trans e2 @ [Sm5.SUB]
    | K.MUL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.MUL]
    | K.DIV (e1, e2) -> trans e1 @ trans e2 @ [Sm5.DIV]
    | K.EQUAL (e1, e2) -> trans e1 @ trans e2 @ [Sm5.EQ]
    | K.LESS (e1, e2) -> trans e1 @ trans e2 @ [Sm5.LESS]
    | K.NOT exp -> trans exp @ [Sm5.NOT]
    | K.ASSIGN (name, exp) -> begin
      trans exp @ [Sm5.PUSH (Sm5.Id name); Sm5.STORE] @
      [Sm5.PUSH (Sm5.Id name); Sm5.LOAD]
    end
    | K.SEQ (ebefore, eafter) -> trans ebefore @ [Sm5.POP] @ trans eafter
    | K.IF (econd, ethen, eelse) -> trans econd @ [Sm5.JTR (trans ethen, trans eelse)]
    | K.LETV (x, e1, e2) -> begin
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    end
    | K.LETF (name, param, ebody, eafter) -> begin
      [Sm5.PUSH (Sm5.Fn (param, [Sm5.BIND name] @ trans ebody)); Sm5.BIND name] @
      trans eafter @ [Sm5.UNBIND; Sm5.POP]
    end
    | K.CALLR (name, param) -> begin
      [Sm5.PUSH (Sm5.Id name)] @ (* 함수 스택 맨 위에 푸쉬 *)
      [Sm5.PUSH (Sm5.Id name)] @ (* 함수 Push *)
      [Sm5.PUSH (Sm5.Id param); Sm5.LOAD] @ (* Value Push *)
      [Sm5.PUSH (Sm5.Id param)] @ (* Location Push *)
      [Sm5.CALL] (* CALL *)
    end
    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE exp -> begin
      let tempname = "α" in
      [Sm5.MALLOC; Sm5.BIND tempname] @ (* 변수 선언 *)
      trans exp @ [Sm5.PUSH (Sm5.Id tempname); Sm5.STORE] @ (* 임시변수에 값 대입 *)
      [Sm5.PUSH (Sm5.Id tempname); Sm5.LOAD; Sm5.PUT] @ (* 값 출력 *)
      [Sm5.PUSH (Sm5.Id tempname); Sm5.LOAD] @ (* 스택 맨 위에 값 저장 *)
      [Sm5.UNBIND; Sm5.POP] (* 임시변수 해제 *)
    end
    | K.WHILE _ | K.FOR _ | K.CALLV _ -> [] (* Unreachable! *)
end
