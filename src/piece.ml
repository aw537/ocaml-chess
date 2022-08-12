exception UnImplemented
exception InvalidMove
exception EmptySpace

type piece_type =
  | Pawn
  | Rook
  | Knight
  | Bishop
  | King
  | Queen

type color =
  | White
  | Black

type t = {
  role : piece_type;
  pos : Pos.t;
  alive : bool;
  has_moved : bool;
  id : string;
  team : color;
  pts : int;
}

let make_id team role number =
  let team_s =
    match team with
    | White -> "W"
    | Black -> "B"
  in
  let role_n = string_of_int number in
  let role_s =
    match role with
    | Pawn -> "P"
    | Rook -> "R"
    | Knight -> "N"
    | Bishop -> "B"
    | King -> "K"
    | Queen -> "Q"
  in
  team_s ^ role_s ^ role_n

let assign team = function
  | Pawn -> if team = White then 1 else -1
  | Rook -> if team = White then 3 else -3
  | Knight -> if team = White then 2 else -2
  | Bishop -> if team = White then 3 else -3
  | Queen -> if team = White then 10 else -10
  | King -> if team = White then 5 else -5

let make_piece role pos number team =
  let id_string = make_id team role number in
  {
    role;
    pos;
    alive = true;
    has_moved = false;
    id = id_string;
    team;
    pts = assign team role;
  }

let get_pos piece = piece.pos
let is_alive piece = piece.alive
let get_id piece = piece.id
let get_has_moved (piece : t) = piece.has_moved
let get_role (piece : t) : piece_type = piece.role

(* [check_block_covered] is true if block at pos is unoccupied in
   curr_board and false otherwise *)
let check_block_covered pos curr_board =
  List.fold_left
    (fun acc curr_piece -> curr_piece.pos <> pos && acc)
    true curr_board

(* [check_blocks] is true if all the positions in blk_lst are unoccupied
   in curr_board and false otherwise *)
let check_blocks blk_lst curr_board =
  List.fold_left
    (fun acc blk -> check_block_covered blk curr_board && acc)
    true blk_lst

(* [piece_at pos brd] is a piece on a [pos] on a [board] Prerequisite:
   there exists a piece at position p1 Raises EmptySpace if a space is
   not occupied *)
let rec piece_at pos brd : t =
  match brd with
  | h :: t -> if get_pos h = pos then h else piece_at pos t
  | _ -> raise EmptySpace

(** [role_to_string pt] is the string shorthand for the type of the
    piece e.g., Pawn is "P"*)
let role_to_string pt =
  match pt with
  | Pawn -> "P"
  | Rook -> "R"
  | Knight -> "N"
  | Bishop -> "B"
  | King -> "K"
  | Queen -> "Q"

(** [command_format p b] is the command notation of [p] on [b] e.g.,
    white pawn at (4,1) with id 1 is represented as "WP1"*)
let command_format pos brd =
  let pce = piece_at pos brd in
  let piece_id = pce.id in
  piece_id

(* [is_empty_or_same new_pos piece_list team] is true if the new_pos is
   either empty or occupied by a piece of the other team, and false
   otherwise *)
let is_empty_or_enemy new_pos piece_list team =
  check_block_covered new_pos piece_list
  || (piece_at new_pos piece_list).team <> team

(* [friendly_piece_at_pos] is true if pieces at pos1 and pos2 are part
   of the same team*)
let friendly_piece_at_pos board pos1 pos2 =
  try (piece_at pos1 board).team = (piece_at pos2 board).team
  with EmptySpace -> false

(* [pawn_move_vertical b p ps] is whether pawn [p] can move up one space
   if white, or down one space as black if nothing is in the way on
   board [b] given a move to [ps]*)
let pawn_move_vertical board pce new_pos =
  let old_pos = pce.pos in
  (Pos.fst old_pos = Pos.fst new_pos
  && Pos.snd old_pos + 1 = Pos.snd new_pos
  && check_block_covered new_pos board)
  && pce.team = White
  || (Pos.fst old_pos = Pos.fst new_pos
     && Pos.snd old_pos - 1 = Pos.snd new_pos
     && check_block_covered new_pos board)
     && pce.team = Black

(* [pawn_up_two b p ps] is whether pawn [p] can move up two spaces if
   nothing is in the way on board [b] and [p] has not moved yet given a
   move to [ps]*)
let pawn_up_two board pce new_pos =
  let old_pos = pce.pos in
  Pos.fst old_pos = Pos.fst new_pos
  && get_has_moved pce = false
  && (pce.team = White
      && Pos.snd old_pos + 2 = Pos.snd new_pos
      && check_blocks
           [
             Pos.add old_pos Pos.one_up;
             Pos.add old_pos (Pos.multiply Pos.one_up 2);
           ]
           board
     || pce.team = Black
        && Pos.snd old_pos - 2 = Pos.snd new_pos
        && check_blocks
             [
               Pos.add old_pos Pos.one_down;
               Pos.add old_pos (Pos.multiply Pos.one_down 2);
             ]
             board)

(* [pawn_move_diagonal] is whether pawn [p] can move diagonally to
   capture an enemy piece in a diagonal space on board [b] given a move
   to [ps] *)
let pawn_move_diagonal board pce new_pos =
  let old_pos = pce.pos in
  check_block_covered new_pos board = false
  && (pce.team = White
      && new_pos = Pos.add old_pos (Pos.add Pos.one_up Pos.one_to_right)
     || pce.team = White
        && new_pos
           = Pos.add old_pos (Pos.add Pos.one_up Pos.one_to_left)
     || pce.team = Black
        && new_pos
           = Pos.add old_pos (Pos.add Pos.one_down Pos.one_to_right)
     || pce.team = Black
        && new_pos
           = Pos.add old_pos (Pos.add Pos.one_down Pos.one_to_left))

let update_pawn board pce new_pos =
  let old_pos = pce.pos in
  (* Check if position is occupied by a piece on the same team, if it is
     then return false*)
  (not (friendly_piece_at_pos board old_pos new_pos))
  && (pawn_move_vertical board pce new_pos
     || pawn_up_two board pce new_pos
     || pawn_move_diagonal board pce new_pos)

(* [add_pos_iterator step_pos start_pos curr_num] is a list of curr_num
   positions where each iteration takes a step in step_pos. Does not
   include start_pos. *)
let rec add_pos_iterator step_pos start_pos curr_num =
  if curr_num > 0 then
    Pos.add start_pos (Pos.multiply step_pos curr_num)
    :: add_pos_iterator step_pos start_pos (curr_num - 1)
  else []

(* [iterate_over_difference] checks all the positions in
   add_pos_iterator using the difference of the positions as a step.
   Created as a helper function for bishop and rook. *)
let iterate_over_difference new_pos old_pos piece_list =
  let difference = Pos.minus new_pos old_pos in
  let iter_num = difference |> Pos.non_zero_entry |> abs in
  check_blocks
    (add_pos_iterator (Pos.make_ones difference) old_pos (iter_num - 1))
    piece_list

(* [check_valid_for_straight_line] checks if a move is valid for a
   straight-moving piece, where the line criteria is given by line_crit.
   Made as a helper function for rook and bishop. *)
let check_valid_for_straight_line
    piece_list
    piece
    new_pos
    (line_crit : Pos.t -> Pos.t -> bool) =
  let old_pos = piece.pos in
  let team = piece.team in
  (* Check that new_pos is in line with old_pos *)
  line_crit new_pos old_pos
  (* Check that all the blocks between old_pos and new_pos are empty *)
  && iterate_over_difference new_pos old_pos piece_list
  && (* Check that new_pos is either empty to inhabited by the different
        team *)
  is_empty_or_enemy new_pos piece_list team

let update_knight piece_list piece new_pos =
  let valid =
    [
      (-2, 1);
      (-1, 2);
      (1, 2);
      (2, 1);
      (2, -1);
      (1, -2);
      (-1, -2);
      (-2, -1);
    ]
    |> List.map (fun coord ->
           (Pos.pos : int -> int -> Pos.t) (fst coord) (snd coord))
    |> List.map (fun p -> Pos.add piece.pos p)
    |> List.filter (fun p ->
           let x = Pos.fst p in
           let y = Pos.snd p in
           x < 8 && x >= 0 && y < 8 && y >= 0)
    |> List.mem new_pos
  in
  if valid then
    match piece_at new_pos piece_list with
    | exception EmptySpace -> true
    | p -> p.team <> piece.team
  else false

let update_rook piece_list piece new_pos =
  check_valid_for_straight_line piece_list piece new_pos
    (fun new_pos old_pos ->
      Pos.fst old_pos = Pos.fst new_pos
      || Pos.snd old_pos = Pos.snd new_pos)

let update_bishop piece_list piece new_pos =
  check_valid_for_straight_line piece_list piece new_pos
    (fun new_pos old_pos ->
      let difference = Pos.minus new_pos old_pos in
      Pos.(abs (fst difference) = abs (snd difference)))

let update_queen piece_list piece new_pos =
  update_rook piece_list piece new_pos
  || update_bishop piece_list piece new_pos

let update_king piece_list piece new_pos =
  let valid =
    [
      (-1, 0);
      (1, 0);
      (0, 1);
      (0, -1);
      (-1, 1);
      (1, -1);
      (1, 1);
      (-1, -1);
    ]
    |> List.map (fun coord ->
           (Pos.pos : int -> int -> Pos.t) (fst coord) (snd coord))
    |> List.map (fun p -> Pos.add piece.pos p)
    |> List.filter (fun p ->
           let x = Pos.fst p in
           let y = Pos.snd p in
           x < 8 && x >= 0 && y < 8 && y >= 0)
    |> List.mem new_pos
  in
  if valid then
    match piece_at new_pos piece_list with
    | exception EmptySpace -> true
    | p -> p.team <> piece.team
  else false

let is_valid_move piece_list piece new_pos =
  let alive_piece_list =
    List.filter (fun piece -> is_alive piece) piece_list
  in
  (* Check if movement is no movement *)
  new_pos <> piece.pos
  (* Check if movement is within bounds *)
  && Pos.(
       fst new_pos >= 0
       && fst new_pos <= 7
       && snd new_pos >= 0
       && snd new_pos <= 7)
  &&
  (* Check movement for specific piece type *)
  match piece.role with
  | Pawn -> update_pawn alive_piece_list piece new_pos
  | Knight -> update_knight alive_piece_list piece new_pos
  | Rook -> update_rook alive_piece_list piece new_pos
  | Bishop -> update_bishop alive_piece_list piece new_pos
  | Queen -> update_queen alive_piece_list piece new_pos
  | King -> update_king alive_piece_list piece new_pos

let is_valid_castle piece_list king rook king_side =
  let rook_destination =
    0 |> Pos.pos (if king_side then -2 else 3) |> Pos.add rook.pos
  in
  (not king.has_moved) && (not rook.has_moved)
  && is_valid_move piece_list rook rook_destination

let can_be_promoted piece =
  piece.role = Pawn && piece.alive
  && ((piece.team = White && Pos.snd piece.pos = 7)
     || (piece.team = Black && Pos.snd piece.pos = 0))

let promote piece role number =
  let new_id = make_id piece.team role number in
  { piece with role; id = new_id }

let move_piece piece new_pos =
  { piece with has_moved = true; pos = new_pos }

let piece_to_string piece =
  match piece.role with
  | Pawn -> if piece.team = White then "♙" else "♟"
  | Rook -> if piece.team = White then "♖" else "♜"
  | Knight -> if piece.team = White then "♘" else "♞"
  | Queen -> if piece.team = White then "♕" else "♛"
  | Bishop -> if piece.team = White then "♗" else "♝"
  | King -> if piece.team = White then "♔" else "♚"

let kill piece = { piece with alive = false }
let get_team piece = piece.team

let rec num_role brd role team =
  match brd with
  | [] -> 0
  | h :: t ->
      if h.role = role && h.team = team then 1 + num_role t role team
      else num_role t role team

let get_points p = p.pts

let pts_at_pos pos board =
  (* add strategic pts *)
  try
    let piece = piece_at pos board in
    piece.pts
  with EmptySpace -> 0
