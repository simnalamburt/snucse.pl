(*
 * SNU 4190.310 Programming Languages
 * Homework "Rozetta" Skeleton
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

let trans_v (input: Sm5.value): Sonata.value =
  match input with
  | Sm5.Z z -> Sonata.Z z
  | Sm5.B b -> Sonata.B b
  | Sm5.L _ -> raise (Sonata.Error "Invalid input program : pushing location")
  | Sm5.Unit -> Sonata.Unit
  | Sm5.R _ -> raise (Sonata.Error "Invalid input program : pushing record")

let trans_obj (input: Sm5.obj): Sonata.obj =
  match input with
  | Sm5.Val v -> Sonata.Val (trans_v v)
  | Sm5.Id id -> Sonata.Id id
  | Sm5.Fn (arg, command) -> begin
    (* TODO *)
    failwith "TODO : fill in here"
  end

let rec trans_cmd (input: Sm5.cmd): Sonata.command =
  match input with
  | Sm5.PUSH obj  -> [Sonata.PUSH (trans_obj obj)]
  | Sm5.POP       -> [Sonata.POP]
  | Sm5.STORE     -> [Sonata.STORE]
  | Sm5.LOAD      -> [Sonata.LOAD]
  | Sm5.MALLOC    -> [Sonata.MALLOC]
  | Sm5.BOX z     -> [Sonata.BOX z]
  | Sm5.UNBOX id  -> [Sonata.UNBOX id]
  | Sm5.BIND id   -> [Sonata.BIND id]
  | Sm5.UNBIND    -> [Sonata.UNBIND]
  | Sm5.GET       -> [Sonata.GET]
  | Sm5.PUT       -> [Sonata.PUT]
  | Sm5.ADD       -> [Sonata.ADD]
  | Sm5.SUB       -> [Sonata.SUB]
  | Sm5.MUL       -> [Sonata.MUL]
  | Sm5.DIV       -> [Sonata.DIV]
  | Sm5.EQ        -> [Sonata.EQ]
  | Sm5.LESS      -> [Sonata.LESS]
  | Sm5.NOT       -> [Sonata.NOT]
  | Sm5.JTR (a,b) -> [Sonata.JTR (trans a, trans b)]
  | Sm5.CALL -> begin
    (* TODO *)
    failwith "TODO : fill in here"
  end
and trans (input: Sm5.command): Sonata.command =
  match input with
  | [] -> []
  | first::rest -> trans_cmd first @ trans rest
