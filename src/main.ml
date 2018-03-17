open Eval

module LoopCtrl = struct
  exception Quit
  exception Run
  exception Exit
  exception Exit0
end

let rec loop ?(running = false) history stack instrs =
  try
    if running then
      let history', stack', instrs' =
        match instrs with
        | i :: _ ->
          let () = print_endline @@ show_instr i in
          let stack', instrs' = eval0 (stack, instrs) in
          i :: history, stack', instrs'
        | _ -> print_endline "(exit)"; raise LoopCtrl.Exit0
      in
      loop ~running history' stack' instrs'
    else
      let () = print_string ">> " in
      let line =
        try read_line () with
        | End_of_file -> raise LoopCtrl.Quit
      in
      let history', stack', instrs' =
        match line with
        | "show_stack" | "shows" ->
          let () = print_endline @@ show_stack stack in
          history, stack, instrs
        | "show_instrs" | "showi" ->
          let () = print_endline @@ show_instrlist instrs in
          history, stack, instrs
        | "show_history" | "showh" ->
          let () = print_endline @@ show_instrlist history in
          history, stack, instrs
        | "next" | "n" ->
          begin match instrs with
            | i :: _ ->
              let () = print_endline @@ show_instr i in
              let stack', instrs' = eval0 (stack, instrs) in
              i :: history, stack', instrs'
            | _ -> print_endline "(exit)"; raise LoopCtrl.Exit
          end
        | "run" | "r"  -> raise LoopCtrl.Run
        | "?" | "help" | "h" ->
          let () = print_endline "  show_stack   | shows    show stack
  show_instrs  | showi    show instructions
  show_history | showh    show history
  next         | n        evaluate 1 instruction
  quit         | q        quit
  help   |  h  | ?        show it"
          in
          history, stack, instrs
        | "quit" | "q" -> raise LoopCtrl.Quit
        | _ -> history, stack, instrs
      in
      loop ~running history' stack' instrs'
  with
  | LoopCtrl.Quit -> print_endline "quit"
  | LoopCtrl.Exit -> loop ~running history stack instrs
  | LoopCtrl.Run -> loop ~running: true history stack instrs
  | LoopCtrl.Exit0 -> loop ~running: false history stack instrs


let stack = []
let instrs_example = [Push(Int 3); Push(Int 5); Push(Int 10); Push(Int 4); Push(Int 2); Add; Swp; Sub; Mul; Div]

let () = loop [] stack instrs_example

