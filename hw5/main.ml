(*
 * SNU 4190.310 Programming Languages 
 *
 * SM5
 *)

open Translate
open Pp
open Sm5
open K

let main () =
  let pp = ref false in
	let k = ref false in
  let src = ref "" in
  let _ =
      Arg.parse
        [("-pp", Arg.Set pp, "display parse tree");
	      ("-k", Arg.Set k, "run using K interpreter");
	      ("-gc", Arg.Set Sm5.gc_mode, "run with garbage collection")]
        (fun x -> src := x)
        ("Usage: " ^ (Filename.basename Sys.argv.(0)) ^ " [-ptree] [file]")
  in
  let lexbuf = Lexing.from_channel (if !src = "" then stdin else open_in !src) in
  let pgm = Parser.program Lexer.start lexbuf in
   
	if !pp then 
    KParseTreePrinter.print pgm
	else if !k then 
    ignore (K.run pgm)
  else 
    ignore (Sm5.run(Translator.trans pgm))

let _ = main ()
