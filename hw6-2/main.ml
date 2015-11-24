(*
 * SNU 4190.310 Programming Languages 
 * Main driver of homework "Continuation Passing Style"
 * Jaeseung Choi (jschoi@ropas.snu.ac.kr)
 *)

let main () =
  let print_m = ref false in
  let print_cps = ref false in
  let src = ref "" in
  let _ = 
    Arg.parse 
      [("-pp", Arg.Set print_m, "Print M0 program");
      ("-pcps", Arg.Set print_cps, "Print CPS-converted  program")
      ]
      (fun x -> src := x)
      "Usage: ./run [<options>] <M0 file>"
  in  
  
  let lexbuf = 
    Lexing.from_channel (if !src = "" then stdin else open_in !src) 
  in
  let pgm = Parser.program Lexer.start lexbuf in
  let _ = print_newline() in
  if !print_m then (
    let _ = print_endline "== Input Program ==" in
    let _ = M0.print pgm in
    print_newline()
  );
  let cps_pgm = Cps.cps pgm in
  if !print_cps then (
    let _ = print_endline "== CPS-converted Program ==" in
    let _ = M0.print cps_pgm in
    print_newline()
  );
  let _ = print_endline "== Running Input Program with M0 Interpreter ==" in
  let orig_result = M0.run pgm in
  let _ = 
    match orig_result with
    |M0.N n -> print_endline (string_of_int n)
    | _ -> failwith "Program is not evaluated to a number"
  in
  let _ = print_endline "== Running converted program with M0 Interpreter ==" in
  let cps_result = M0.run (M0.App (cps_pgm, M0.Fn ("v", M0.Var "v"))) in
  let _ = 
    match cps_result with
    |M0.N n -> print_endline (string_of_int n)
    | _ -> failwith "Program is not evaluated to a number"
  in
  ()

let _ = main ()
