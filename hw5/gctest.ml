open Sm5.Sm5

let _ = gc_mode := true

let check_exception cmd =
  try let _ = run cmd in false with GC_Failure -> true

(* concat command n times *)
let append (n: int) (f: int -> command) (cmd: command) : command =
  let rec iter i =
    if i = n then []
    else (f i) @ iter (i + 1) in cmd @ (iter 0)


(* 1. Simple malloc & use : trigger gc and success *)
let cmds1 =
    (* To be collected *)
    let cmds = [
        PUSH (Val (Z 1));
        MALLOC;
        STORE;
    ] in

    let cmds = append 127 (fun i ->
        let v = Printf.sprintf "x%d" i in [
            MALLOC;
            BIND v;
            PUSH (Val (Z 1));
            PUSH (Id v);
            STORE;
        ]) cmds in

    (* Trigger GC *)
    let cmds = cmds @ [
        MALLOC;
        BIND "x_new";
        PUSH (Val (Z 10));
        PUSH (Id "x_new");
        STORE;

        PUSH (Id "x_new");
        LOAD;
    ] in

    (* Check if allocated memory location's values are not affected by GC() *)
    let cmds = append 127 (fun i ->
        let v = Printf.sprintf "x%d" i in [
            PUSH (Id v);
            LOAD;
            ADD;
         ]) cmds in

    let cmds = cmds @ [PUT] in

    cmds


(* 2. Simple malloc & use : gc fails *)
let cmds2 =
    let cmds = append 128 (fun _ -> [
        MALLOC;
        BIND "x";
        PUSH (Val (Z 200));
        PUSH (Id "x");
        STORE;
        ]
    ) [] in

    let cmds = cmds @ [
        (* Trigger GC *)
        PUSH (Val (Z 400));
        MALLOC;
        STORE;
        ] in

    (* Access all the allocated memory locations, ensuring they must not have been collected *)
    let cmds = append 128 (fun _ -> [
        PUSH (Id "x");
        LOAD;
        POP;

        UNBIND;
        POP;
        ]
    ) cmds in
    cmds

(* 3. Gc must be able to track record : gc fail *)
let cmds3 =
  let cmds = append 126 (fun i ->
      let v = Printf.sprintf "x%d" i in [
        MALLOC;
        BIND v;
        PUSH (Val (Z i));
        PUSH (Id v);
        STORE;
      ])
  [] in

  let cmds = cmds @ [
    MALLOC;
    BIND "loc";

    PUSH (Val (Z 1));
    PUSH (Id "loc");
    STORE;

    UNBIND;
    BOX 1
    ]
  in

  let cmds = cmds @ [
    MALLOC;
    BIND "box";

    PUSH (Id "box");
    STORE;

    (* Trigger GC *)
    PUSH (Val (Z 1));
    MALLOC;
    STORE;
  ] in

  (* Access all the allocated memory locations, ensuring they must not have been collected *)
  let cmds = append 126 (fun i ->
      let v = Printf.sprintf "x%d" i in [
          PUSH (Id v);
          LOAD;
          POP;
       ]) cmds @ [PUSH (Id "box"); LOAD; UNBOX "loc"; LOAD] in

  cmds


(* 4. GC must be able to track locations in the 'Continuation' : gc fails *)
let cmds4 =
    let cmds = [
        PUSH (Fn ("x", [
            (* Trigger GC *)
            PUSH (Val (Z 1));
            MALLOC;
            STORE;

            (* Access argument location, ensuring it must not have been collected *)
            PUSH (Id "x");
            LOAD;
            POP;
        ]));

        BIND "f";
    ] in

    let cmds = append 127 (fun i ->
        let v = Printf.sprintf "x%d" i in [
            MALLOC;
            BIND v;
            PUSH (Val (Z i));
            PUSH (Id v);
            STORE
        ]) cmds in

    let cmds = cmds @ [
        PUSH (Id "f");
        PUSH (Val (Z 1));
        MALLOC;
        CALL;

    ] in

    (* Access all the allocated memory locations, ensuring they must not have been collected *)
    let cmds = append 127 (fun i ->
        let v = Printf.sprintf "x%d" i in [
            PUSH (Id v);
            LOAD;
            POP;
         ]) cmds in

    cmds


(* Location allocated in function can be collected after return : gc success *)
let cmds5 =
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

    let cmds = append 126 (fun i ->
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
      append 126
        (fun i ->
          let v = Printf.sprintf "x%d" i in
            [PUSH (Id v);
            LOAD;
            ADD]
        ) (cmds @ [PUSH (Val (Z 0));]) in

    let cmds = cmds @ [PUT] in
    cmds


let _ = run cmds1 (* 137 *)
let _ = print_endline (string_of_bool (check_exception cmds2)) (* true *)
let _ = print_endline (string_of_bool (check_exception cmds3)) (* true *)
let _ = print_endline (string_of_bool (check_exception cmds4)) (* true *)
let _ = run cmds5 (* 630 *)

let cmds6 =
    let cmds = [] in

    let cmds = append 126 (fun i ->
        let v = Printf.sprintf "x%d" i in [
            MALLOC;
            BIND v;
            PUSH (Val (Z 5));
            PUSH (Id v);
            STORE;
        ]) cmds in

    let cmds = cmds @ [
        MALLOC;
        BIND "TARGET";
        PUSH (Val (Z 1234));
        PUSH (Id "TARGET");
        STORE;
        UNBIND;

        PUSH (Val (Z 10));
        MALLOC;
        STORE;
        (* Trigger GC *)
        PUSH (Val (Z 10));
        MALLOC;
        STORE;
        BOX 1;
        UNBOX "TARGET";
        LOAD;
    ] in

    (* Check if allocated memory location's values are not affected by GC() *)
    let cmds =
      append 126
        (fun i ->
          let v = Printf.sprintf "x%d" i in
            [PUSH (Id v);
            LOAD;
            POP]
        ) (cmds) in

    let cmds = cmds @ [PUT] in
    cmds

let _ = run cmds6 (* 1234 *)

let cmds7 =
    let cmds = [
        MALLOC;
        BIND "val";
        PUSH (Val (Z 1234));
        PUSH (Id "val");
        STORE;
        UNBIND;
    ] in

    let cmds = append 126 (fun i ->
        let v = Printf.sprintf "x%d" i in [
            BOX 1;
            MALLOC;
            BIND v;
            PUSH (Id v);
            STORE;
            UNBIND;
        ]) cmds in

    let cmds = cmds @ [
        BOX 1;
        MALLOC;
        POP;

        (* Trigger GC *)
        MALLOC;
        POP;
    ] in

    (* Check if allocated memory location's values are not affected by GC() *)
    let cmds =
      append 126
        (fun i ->
          let v = Printf.sprintf "x%d" (125 - i) in
            [UNBOX v;
            LOAD;]
        ) (cmds) in

    let cmds = cmds @ [UNBOX "val"; LOAD; PUT] in
    cmds

let _ = run cmds7 (* 1234 *)
