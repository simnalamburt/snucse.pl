(*
 * SNU 4190.310 Programming Languages 
 * SM5 Parser 
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)


(* Hand-written without lex/yacc, since SM5 language is simple *)

open Str

let buf = Buffer.create 256

let rec tokenize input_str accum_tokens =
  if input_str = "" then accum_tokens else
    let idx = 
      try search_forward (regexp "[][ \t\n(),;]") input_str 0 with
      Not_found -> 
        let error_msg = Printf.sprintf "delim token unfound :\n%s" input_str in
        failwith ("Parsing error : " ^ error_msg)
    in
    let token, rest_str = 
      if idx = 0 then
        (string_before input_str 1, string_after input_str 1)
      else
        (string_before input_str idx, string_after input_str idx)
    in
    let accum_tokens = 
      match token with
      | " " | "\t" | "\n" -> accum_tokens
      | _ -> token :: accum_tokens
    in
    tokenize rest_str accum_tokens

let consume token token_list = 
  match token_list with
  | head_t :: tail_tokens -> 
    if head_t = token then tail_tokens else 
      let fail_msg = Printf.sprintf "%s expected instead of %s" token head_t in
      failwith ("Token not match : " ^ fail_msg)
  | [] -> failwith "Unexpected end of program"

let parse_obj token = 
  match token with
  | "true" -> Sm5.Val (Sm5.B true)
  | "false" -> Sm5.Val (Sm5.B false)
  | "unit" -> Sm5.Val Sm5.Unit
  | _ -> 
    try Sm5.Val (Sm5.Z (int_of_string token)) with 
    Failure _ -> Sm5.Id token

let rec parse_cmd token_list = 
  match token_list with
  | "pop" :: tail_token -> (Sm5.POP, tail_token)
  | "store" :: tail_token -> (Sm5.STORE, tail_token)
  | "load" :: tail_token -> (Sm5.LOAD, tail_token) 
  | "malloc" :: tail_token -> (Sm5.MALLOC, tail_token)
  | "box" :: n_str :: tail_token -> 
    let n = 
      try int_of_string n_str with 
      Failure _ -> failwith "Non-integer after box command" 
    in
    (Sm5.BOX n, tail_token)
  | "unbox" :: id :: tail_token -> 
    (Sm5.UNBOX id, tail_token)
  | "bind" :: id :: tail_token -> 
    (Sm5.BIND id, tail_token)
  | "unbind" :: tail_token -> (Sm5.UNBIND, tail_token)
  | "get" :: tail_token -> (Sm5.GET, tail_token)
  | "put" :: tail_token -> (Sm5.PUT, tail_token)
  | "call" :: tail_token -> (Sm5.CALL, tail_token)
  | "add" :: tail_token -> (Sm5.ADD, tail_token)
  | "sub" :: tail_token -> (Sm5.SUB, tail_token)
  | "mul" :: tail_token -> (Sm5.MUL, tail_token)
  | "div" :: tail_token -> (Sm5.DIV, tail_token)
  | "eq" :: tail_token -> (Sm5.EQ, tail_token)
  | "less" :: tail_token -> (Sm5.LESS, tail_token)
  | "not" :: tail_token -> (Sm5.NOT, tail_token)
  | "jtr" :: "(" :: tail_token ->
    let (command_t, tail_token) = parse_command tail_token in
    let tail_token = consume "," tail_token in
    let (command_f, tail_token) = parse_command tail_token in
    let tail_token = consume ")" tail_token in
    (Sm5.JTR (command_t, command_f), tail_token)
  | "push" :: "(" :: arg_id :: "," :: tail_token ->
    let (fun_comm, tail_token) = parse_command tail_token in
    let tail_token = consume ")" tail_token in
    (Sm5.PUSH (Sm5.Fn (arg_id, fun_comm)), tail_token)
  | "push" :: token :: tail_token ->
    (Sm5.PUSH (parse_obj token), tail_token)
  | _ -> 
    let _ = List.iter print_endline token_list in
    failwith "parsing error : unexpected token sequence"

and parse_command_helper accum_command token_list = 
  match token_list with
  | "]" :: tail_token -> (accum_command, tail_token)
  | "[" :: tail_token | ";" :: tail_token -> 
    let (cmd, tail_token) = parse_cmd tail_token in
    parse_command_helper (accum_command @ [cmd]) tail_token
  | _ -> 
    let _ = List.iter print_endline token_list in
    failwith "parsing error : unexpected token btw. cmds"

and parse_command token_list =
  parse_command_helper [] token_list

let parse_sm5 : string -> Sm5.command = fun pgm_str ->
  let token_list = List.rev (tokenize pgm_str []) in
  let (pgm, tail_token) = parse_command token_list in
  if List.length tail_token = 0 then
    pgm
  else
    failwith "parsing error : trailing tokens after the end of program"
