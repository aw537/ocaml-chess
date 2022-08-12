exception InvalidCommand
exception UnImplemented

type t = int * int

let pos int1 int2 = (int1, int2)
let fst pos = fst pos
let snd pos = snd pos
let add pos1 pos2 = (fst pos1 + fst pos2, snd pos1 + snd pos2)
let multiply pos1 scalar = (scalar * fst pos1, scalar * snd pos1)
let minus pos1 pos2 = add pos1 (multiply pos2 (-1))
let non_zero_entry pos = if fst pos <> 0 then fst pos else snd pos

(* A helper function to implement [make_ones] for one specific
   element *)
let make_ones_helper elem =
  if elem > 0 then 1 else if elem < 0 then -1 else 0

let make_ones pos =
  (make_ones_helper (fst pos), make_ones_helper (snd pos))

let one_to_right : t = (1, 0)
let one_to_left : t = (-1, 0)
let one_up : t = (0, 1)
let one_down : t = (0, -1)

let get_all_pos : t list =
  let rec all_pos_helper iter =
    if iter = 0 then [ pos 0 0 ]
    else pos (iter / 8) (iter mod 8) :: all_pos_helper (iter - 1)
  in
  all_pos_helper 63
