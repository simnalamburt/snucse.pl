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
  try
    let _ = Error.init () in
    let lexbuf = 
      Lexing.from_channel (if !src = "" then stdin else open_in !src) 
    in
    let pgm = Parser.program Lexer.start lexbuf in  
    if !print_m then (
      let _ = print_string "== Input Program ==\n" in
      let _ = M_Printer.print pgm in
      print_newline()
    );
    let _ = print_string "== Running with M Interpreter ==\n" in
    M.run pgm
  with v -> Error.handle_exn v

let _ = Printexc.catch run () 
