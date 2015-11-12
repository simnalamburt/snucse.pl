(*
 * SNU 4190.310 Programming Languages
 * K-- to SM5 translator skeleton code
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

open K
open Sm5
module Translator = struct
  let rec trans (input: K.program): Sm5.command =
    match input with
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

    | K.WHILE (econd, ebody)
    -> failwith "WHILE Unimplemented"
    | K.FOR (econd, efrom, eto, ebody)
    -> failwith "FOR Unimplemented"

    | K.LETV (x, e1, e2) -> begin
      trans e1 @ [Sm5.MALLOC; Sm5.BIND x; Sm5.PUSH (Sm5.Id x); Sm5.STORE] @
      trans e2 @ [Sm5.UNBIND; Sm5.POP]
    end
    | K.LETF (name, param, ebody, eafter) -> begin
      [Sm5.PUSH (Sm5.Fn (param, trans ebody)); Sm5.BIND name] @
      trans eafter @ [Sm5.UNBIND; Sm5.POP]
    end

    | K.CALLV (name, eparam)
    -> failwith "CALLV unimplemented"
    | K.CALLR (name, param)
    -> failwith "CALLR Unimplemented"

    | K.READ x -> [Sm5.GET; Sm5.PUSH (Sm5.Id x); Sm5.STORE; Sm5.PUSH (Sm5.Id x); Sm5.LOAD]
    | K.WRITE exp -> begin
      let tempname = "α" in
      [Sm5.MALLOC; Sm5.BIND tempname] @ (* 변수 선언 *)
      trans exp @ [Sm5.PUSH (Sm5.Id tempname); Sm5.STORE] @ (* 임시변수에 값 대입 *)
      [Sm5.PUSH (Sm5.Id tempname); Sm5.LOAD; Sm5.PUT] @ (* 값 출력 *)
      [Sm5.PUSH (Sm5.Id tempname); Sm5.LOAD] @ (* 스택 맨 위에 값 저장 *)
      [Sm5.UNBIND; Sm5.POP] (* 임시변수 해제 *)
    end
end
