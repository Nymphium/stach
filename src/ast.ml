type instrlist = instr list
and stack = data list
and instr =
  | Push of data
  | Pop
  | Dup
  | Swp
  | Add
  | Sub
  | Mul
  | Div
  | Neg
and data =
  | Int of int
[@@deriving show { with_path = false }]

