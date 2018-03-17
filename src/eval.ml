include Ast

exception Lack_stack

let pop = function
  | data :: stack -> (data, stack)
  | [] -> raise Lack_stack

let swap = function
  | d1 :: d2 :: stack -> d2 :: d1 :: stack
  | _ -> raise Lack_stack

let binapply op = function
  | (Int d1) :: (Int d2) :: stack ->
    Int(op d1 d2) :: stack
  | _ -> raise Lack_stack
let uniapply op = function
  | (Int d) :: stack -> Int(op d) :: stack
  | [] -> raise Lack_stack

let eval0 (stack, instrs) =
  match instrs with
  | [] -> (stack, [])
  | instr :: instrs' ->
    match instr with
    | Push p -> p :: stack, instrs'
    | Pop -> pop stack |> snd, instrs'
    | Dup ->
      let data, stack' = pop stack in
      data :: data :: stack, instrs'
    | Swp -> swap stack, instrs'
    | Add -> binapply (+) stack , instrs'
    | Sub -> binapply (-) stack , instrs'
    | Mul -> binapply ( * ) stack , instrs'
    | Div -> binapply (/) stack , instrs'
    | Neg -> uniapply (fun i -> -i) stack, instrs'

