(*
 * SNU 4190.310 Programming Languages 
 *
 * Main Interface for M
 *)

open M
open Pp


let run () =
  let print_m = ref false in
  let src = ref "" in
  let _ = 
    Arg.parse 
      [("-pp", Arg.Set print_m, "Print M program")]
      (fun x -> src := x)
      "Usage: ./run [<options>] <M file>"
  in  
  let _ = Error.init () in
  let chan = if !src = "" then stdin else open_in !src in
  let lexbuf = Lexing.from_channel chan in
  let pgm = Parser.program Lexer.start lexbuf in
  if !print_m then (
    let _ = print_string "== Input Program ==\n" in
    let _ = M_Printer.print_exp pgm in
    print_newline()
  );
  try M_Printer.print_typ (Poly_checker.check pgm) with
  M.TypeError _ -> print_endline "Type Checking Failed" 

let _ = Printexc.catch run () 
