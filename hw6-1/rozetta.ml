(*
 * SNU 4190.310 Programming Languages
 * Homework "Rozetta" Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)


let trans (input: Sm5.command): Sonata.command =
  let dummy: string = "κενός" in (* Sonata.Unit를 저장할 더미 변수 *)
  let trans_value (input: Sm5.value): Sonata.value =
    match input with
    | Sm5.Z z -> Sonata.Z z
    | Sm5.B b -> Sonata.B b
    | Sm5.Unit -> Sonata.Unit
    | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
    | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")
  in
  let rec trans_obj (input: Sm5.obj): Sonata.obj =
    match input with
    | Sm5.Val v -> Sonata.Val (trans_value v)
    | Sm5.Id id -> Sonata.Id id
    | Sm5.Fn (arg, cbody) -> begin
      (* cbody 뒤에, 스택에 있는 Return Address를 Sonata.CALL 하는 코드 삽입 *)
      let return: string = "γ" in (* Return Address가 Sonata.BIND될 이름 *)
      let cbody = [
        (* 1.  Sonata.CALL 직전, 미리 깔아줬던 Return Address 프로시저를 임의의
         *     이름에 Sonata.BIND 한다. *)
        Sonata.BIND return;
        ] @ trans_command cbody @ [
        Sonata.PUSH (Sonata.Id return); Sonata.UNBIND; Sonata.POP;
        Sonata.PUSH (Sonata.Val Sonata.Unit);
        Sonata.PUSH (Sonata.Id dummy);
        Sonata.CALL;
      ] in
      Sonata.Fn (arg, cbody)
    end
  and trans_command (input: Sm5.command): Sonata.command =
    match input with
    | [] -> []
    | Sm5.CALL :: [] -> [Sonata.CALL]
    | Sm5.CALL :: tail -> begin
      (*
       * Sm5.CALL 뒤에 추가적인 호출이 들어갈경우, Return Address 보존이
       * 필요하다.
       *
       * 스택 구조
       *
       *     loc :: value :: proc :: S
       *
       * CALL 하기 전에, Return Address를 미리 스택에 깔아줘야한다.
       *)
      (* 1.  스택에 있는 CALL의 파라미터 3개를 뽑아서 어딘가에 저장한다. 이때
       *     loc과 proc은 tmp에 저장하고, value는 dummy에 저장한다. *)
      let tmp: string = "α" in
      let unused: string = "∅" in
      [Sonata.BIND tmp] @
      [Sonata.PUSH (Sonata.Id dummy); Sonata.STORE] @
      [Sonata.BIND tmp] @
      (* 2.  Return Address를 함수로 만들어서 스택에 push *)
      [Sonata.PUSH (Sonata.Fn (unused, trans_command tail))] @
      (* 3.  어딘가에 저장했던 CALL의 파라미터를 다시 복구 *)
      [Sonata.PUSH (Sonata.Id tmp); Sonata.UNBIND; Sonata.POP] @
      [Sonata.PUSH (Sonata.Id dummy); Sonata.LOAD] @
      [Sonata.PUSH (Sonata.Id tmp); Sonata.UNBIND; Sonata.POP] @
      [Sonata.CALL]
    end
    | cmd :: tail -> begin
      let head = match cmd with
      | Sm5.PUSH obj  -> Sonata.PUSH (trans_obj obj)
      | Sm5.POP       -> Sonata.POP
      | Sm5.STORE     -> Sonata.STORE
      | Sm5.LOAD      -> Sonata.LOAD
      | Sm5.MALLOC    -> Sonata.MALLOC
      | Sm5.BOX z     -> Sonata.BOX z
      | Sm5.UNBOX id  -> Sonata.UNBOX id
      | Sm5.BIND id   -> Sonata.BIND id
      | Sm5.UNBIND    -> Sonata.UNBIND
      | Sm5.GET       -> Sonata.GET
      | Sm5.PUT       -> Sonata.PUT
      | Sm5.ADD       -> Sonata.ADD
      | Sm5.SUB       -> Sonata.SUB
      | Sm5.MUL       -> Sonata.MUL
      | Sm5.DIV       -> Sonata.DIV
      | Sm5.EQ        -> Sonata.EQ
      | Sm5.LESS      -> Sonata.LESS
      | Sm5.NOT       -> Sonata.NOT
      | Sm5.JTR (a,b) -> Sonata.JTR (trans_command a, trans_command b)
      | Sm5.CALL      -> Sonata.CALL (* Unreachable!! *)
      in
      head :: trans_command tail
    end
  in
  (*
   * Return Address를 Sonata.CALL 할때에는 dummy location이 필요하다. 이를 매번
   * 만들어주지 않고, 프로그램을 시작할때에 하나 만들어서 재활용하도록 하자.
   *)
  [Sonata.MALLOC; Sonata.BIND dummy] @
  trans_command input
