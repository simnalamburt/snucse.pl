module type ZEXPR = sig
  exception Error of string
  type id = string
  type expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr
            | MULT of expr * expr
            | DIVIDE of expr * expr
            | MAX of expr list
            | VAR of id
            | LET of id * expr * expr

  type environment
  type value

  val emptyEnv : environment
  val eval : environment * expr -> value
  val print_value : value -> unit
end

module Zexpr : ZEXPR = struct
  exception Error of string
  type id = string
  type expr = NUM of int
            | PLUS of expr * expr
            | MINUS of expr * expr
            | MULT of expr * expr
            | DIVIDE of expr * expr
            | MAX of expr list
            | VAR of id
            | LET of id * expr * expr

  type value = int

  module Dic = Map.Make(String)
  type environment = value Dic.t

  let emptyEnv = Dic.empty
  let print_value = print_int

  let eval ((env, exp): environment * expr): value =
    let rec calc (env: environment) (exp: expr): value =
      match exp with
      | NUM value -> value
      | PLUS   (left, right) -> (calc env left) + (calc env right)
      | MINUS  (left, right) -> (calc env left) - (calc env right)
      | MULT   (left, right) -> (calc env left) * (calc env right)
      | DIVIDE (left, right) -> (calc env left) / (calc env right)
      | MAX exps -> begin
        match List.map (calc env) exps with
        | [] -> 0
        | values -> List.fold_left max min_int values
      end
      | VAR variable -> begin
        try Dic.find variable env with
        | Not_found -> raise (Error "")
      end
      | LET (name, value, exp) ->
          let value = calc env value in
          let env = Dic.add name value env in
          calc env exp
    in
    calc env exp
end
