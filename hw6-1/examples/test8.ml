open Sm5.Sm5

(* concat command n times *)
let append (n: int) (f: int -> command) (cmd: command) : command =
  let rec iter i =
    if i = n then []
    else (f i) @ iter (i + 1) in cmd @ (iter 0)

(* Location allocated in function can be collected after return : gc success *)
let test_cmds = 
    let cmds = [
        PUSH (Fn ("x", [
            (* To be collected *)
            MALLOC;
            BIND "local"; 
            PUSH (Val (Z 1));
            PUSH (Id "local");
            STORE;

            (* Access argument location, ensuring it must not have been collected *)
            PUSH (Id "x");
            LOAD;
            POP;
        ]));

        BIND "f";
    ] in

    let cmds = append 7 (fun i -> 
        let v = Printf.sprintf "x%d" i in [
            MALLOC; 
            BIND v; 
            PUSH (Val (Z 5));
            PUSH (Id v);
            STORE;
        ]) cmds in

    let cmds = cmds @ [
        PUSH (Id "f");
        PUSH (Val (Z 1));
        MALLOC;
        CALL;

        (* Trigger GC *)
        PUSH (Val (Z 10));
        MALLOC;
        STORE;
    ] in

    (* Check if allocated memory location's values are not affected by GC() *)
    let cmds = 
      append 7 
        (fun i -> 
          let v = Printf.sprintf "x%d" i in 
            [PUSH (Id v);
            LOAD;
            ADD]
        ) (cmds @ [PUSH (Val (Z 0));]) in 

    let cmds = cmds @ [PUT] in
    cmds

let _ = print_endline (command_to_str "" test_cmds)
let _ = run test_cmds (* prints out "35\n" *)
