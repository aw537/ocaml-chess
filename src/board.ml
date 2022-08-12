exception UnImplemented
exception EmptySpace
exception IncorrectId
exception InvalidRole of string

open Timer

(* A board is represented as a list [(piece1, [pos1, pos2, ...]),
   (piece2, [pos1, pos2, ...])] where each element is a tuple containing
   a piece and a list of its possible moves RI: The position list must
   be the current list of available moves *)
type t = (Piece.t * Pos.t list) list

(* [get_board board] is the list of pieces making board *)
let get_board (board : t) : Piece.t list =
  List.map (fun piece -> fst piece) board

(* [get_possible_pos_calculated piece board] is the list of possible
   locations piece can move to on board, calculated by iterating through
   every location *)
let get_possible_pos_calculated (piece : Piece.t) (board : t) :
    Pos.t list =
  let piece_list = get_board board in
  List.filter
    (fun pos -> Piece.is_valid_move piece_list piece pos)
    Pos.get_all_pos

(* [piece_to_lst_entry piece board] is the piece along with all of its
   possible positions that can be used in the representation *)
let piece_to_lst_entry (piece : Piece.t) (piece_list : Piece.t list) :
    Piece.t * Pos.t list =
  let possible_pos =
    if Piece.is_alive piece then
      List.filter
        (fun pos -> Piece.is_valid_move piece_list piece pos)
        Pos.get_all_pos
    else []
  in
  (piece, possible_pos)

(* [alive_board board] is board with dead pieces removed *)
let alive_board (board : t) : t =
  List.filter (fun piece -> Piece.is_alive (fst piece)) board

let rec id_to_piece (board : t) (id : string) : Piece.t =
  match board with
  | [] -> failwith "invalid piece id"
  | h :: t ->
      let piece = fst h in
      if Piece.get_id piece = id && Piece.is_alive piece then piece
      else id_to_piece t id

(* [team_to_king board team] is the king from team. Prerequisite: There
   must be exactly one king for team *)
let team_to_king (board : t) (team : Piece.color) : Piece.t =
  let king_id = Piece.(if team = Black then "BK0" else "WK0") in
  id_to_piece board king_id

(** [is_team_checked b team] is a boolean that indicates whether b is
    currently checked for team *)
let is_team_checked (board : t) (team : Piece.color) : bool =
  let alive_board = alive_board board in
  let king = team_to_king alive_board team in
  let rec is_checking piece_list king_pos =
    match piece_list with
    | [] -> false
    | h :: t ->
        if List.mem king_pos (snd h) then true
        else is_checking t king_pos
  in
  is_checking alive_board (Piece.get_pos king)

let make_board (piece_list : Piece.t list) : t =
  List.map (fun piece -> piece_to_lst_entry piece piece_list) piece_list

(* Refreshed the possible positions for each piece on the board *)
let refresh_board (board : t) : t = make_board (get_board board)

let get_piece_list (board : t) : Piece.t list =
  List.filter (fun piece -> Piece.is_alive piece) (get_board board)

let rec needs_promote (board : t) =
  match board with
  | [] -> None
  | h :: t ->
      let piece = fst h in
      let role = Piece.get_role piece in
      let rank = piece |> Piece.get_pos |> Pos.snd in
      if role = Pawn && (rank = 0 || rank = 7) then Some piece
      else needs_promote t

let rec pieces_with_role (role : Piece.piece_type) (board : t) =
  match board with
  | [] -> 0
  | h :: t ->
      let piece = fst h in
      (if Piece.get_role piece = role then 1 else 0)
      + pieces_with_role role t

(* Helper function for [update] that returns both the new board and
   whether or not the board was edited *)
let rec update_helper (board : t) (new_pos : Pos.t) (id : string) :
    t * bool =
  match board with
  | [] -> ([], false)
  | h :: t ->
      let piece = fst h in
      let recursive, found_id = update_helper t new_pos id in
      if Piece.get_id piece = id && Piece.is_alive piece then
        ((Piece.move_piece piece new_pos, []) :: recursive, true)
      else (h :: recursive, found_id)

(* [kill board piece] is board with piece dead *)
let rec kill (board : t) (piece : Piece.t) : t =
  match board with
  | [] -> []
  | h :: t -> if fst h = piece then t else h :: kill t piece

(* Helper function for [update] to kill pieces at the position id just
   moved to *)
let rec update_helper_kill (board : t) (new_pos : Pos.t) (id : string) :
    t =
  match board with
  | [] -> []
  | h :: t ->
      let piece = fst h in
      let recursive = update_helper_kill t new_pos id in
      if Piece.get_pos piece = new_pos && Piece.get_id piece <> id then
        recursive
      else h :: recursive

let update (board : t) (new_pos : Pos.t) (id : string) : t =
  let new_board, found_id = update_helper board new_pos id in
  let new_board =
    if found_id then update_helper_kill new_board new_pos id
    else new_board
  in
  refresh_board new_board

let rec move (piece : Piece.t) (new_pos : Pos.t) (board : t) : t =
  match board with
  | [] -> raise IncorrectId
  | h :: t ->
      let curr_piece = fst h in
      if curr_piece = piece then
        (Piece.move_piece piece new_pos, []) :: t
      else h :: move piece new_pos t |> refresh_board

let castle
    (board : t)
    (king : Piece.t)
    (rook : Piece.t)
    (king_side : bool) : t =
  let rank = if Piece.get_team king = Piece.Black then 7 else 0 in
  let file = if king_side then 6 else 2 in
  let king_new_pos = Pos.pos file rank in
  let rook_new_pos =
    Pos.pos (if king_side then file - 1 else file + 1) rank
  in
  board |> move king king_new_pos |> move rook rook_new_pos

let rec prom_rec
    (board : t)
    (piece : Piece.t)
    (role : Piece.piece_type)
    (number : int) : t =
  match board with
  | [] -> []
  | h :: t ->
      let curr_piece = fst h in
      (if curr_piece = piece then (Piece.promote piece role number, [])
      else h)
      :: prom_rec t piece role number
      |> refresh_board

let promote (board : t) (piece : Piece.t) (role_st : string) : t =
  let role =
    match role_st with
    | "N" -> Piece.Knight
    | "B" -> Piece.Bishop
    | "R" -> Piece.Rook
    | "Q" -> Piece.Queen
    | _ -> raise (InvalidRole role_st)
  in
  let number = 1 + pieces_with_role role board in
  if Piece.can_be_promoted piece then prom_rec board piece role number
  else board

(* [is_check_at_pos board team pos piece] is whether or not board would
   be checked for team if piece moves to pos *)
let is_check_at_pos
    (board : t)
    (team : Piece.color)
    (pos : Pos.t)
    (piece : Piece.t) : bool =
  let board = update board pos (Piece.get_id piece) in
  is_team_checked board team

(* [remove_check_pos board piece] is piece with movements that move into
   check removed *)
let remove_check_pos (board : t) (piece : Piece.t * Pos.t list) :
    Piece.t * Pos.t list =
  let valid_moves =
    List.filter
      (fun pos ->
        not
          (is_check_at_pos board
             (Piece.get_team (fst piece))
             pos (fst piece)))
      (snd piece)
  in
  (fst piece, valid_moves)

let get_board_with_moves (board : t) : (Piece.t * Pos.t list) list =
  List.map (fun piece -> remove_check_pos board piece) board

(* [no_possible_moves_piece board moveset piece team] is whether or not
   piece with moveset has any possible moves on board, taking into
   account moving into check *)
let no_possible_moves_piece
    (board : t)
    (moveset : Pos.t list)
    (piece : Piece.t)
    (team : Piece.color) : bool =
  List.for_all (fun pos -> is_check_at_pos board team pos piece) moveset

(* [no_possible_moves board team] is whether or not there are no
   possible moves for team on board, taking into account moving into
   check *)
let rec no_possible_moves (board : t) (team : Piece.color) : bool =
  List.for_all
    (fun piece ->
      team <> Piece.get_team (fst piece)
      || no_possible_moves_piece board (snd piece) (fst piece) team)
    board

let get_piece_pos_list board =
  let alive_board = alive_board board in
  List.map (fun piece -> Piece.get_pos piece) (get_board alive_board)

let turn_to_color (turn_num : int) : Piece.color =
  if turn_num mod 2 = 0 then Piece.Black else Piece.White

let turn_to_color_string (turn_num : int) : string =
  match turn_to_color turn_num with
  | Black -> "Black"
  | White -> "White"

(** [is_check b t] is a boolean that indicates whether the board is
    checked on turn_num *)
let is_check board turn_num =
  is_team_checked board (turn_to_color turn_num)

let is_checkmate board turn_num =
  is_check board turn_num
  && no_possible_moves board (turn_to_color turn_num)

let is_stalemate board turn_num =
  (not (is_check board turn_num))
  && no_possible_moves board (turn_to_color turn_num)

(** [color_to_move turn_num] is the string representation of the color
    of the team who's turn it is to move *)
let color_to_move turn_num =
  turn_to_color_string turn_num ^ " to move\n"

(* [to_string_with_filler_function t fill_char] is a string
   representation of board t. The elements in the board spaces are
   filled accordingly to [fill_char], where [fill_char row column] is
   the string we should fill in (row, column) in chess space.
   Precondition: [fill_char] must be exactly one character Note: if
   [turn_count] is "-1", only a board with pieces will be displayed; no
   turn counter, etc. will be displayed *)
let to_string_with_filler_function
    (t : t)
    (fill_char : int -> int -> string)
    (turn_count : int) : string =
  let row = if turn_count = -1 then 11 else 13 in
  let rec board_iterator t row column col_index =
    match row with
    | 1 -> "  a b c d e f g h "
    | 2 | 11 -> " -----------------\n" ^ board_iterator t (row - 1) 1 0
    | 12 -> color_to_move turn_count ^ board_iterator t (row - 1) 1 0
    | 13 ->
        "Turn number : "
        ^ string_of_int turn_count
        ^ "\n"
        ^ board_iterator t (row - 1) 1 0
    | row_num -> begin
        match column with
        | 1 ->
            string_of_int (row_num - 2)
            ^ board_iterator t row_num (column + 1) col_index
        | 18 -> "|\n" ^ board_iterator t (row_num - 1) 1 0
        | n ->
            if n mod 2 = 0 then
              "|" ^ board_iterator t row_num (column + 1) col_index
            else
              fill_char (column - 3 - col_index) (row_num - 3)
              ^ board_iterator t row_num (column + 1) (col_index + 1)
      end
  in
  board_iterator t row 1 1

let timer_string turn_num =
  let time = start_timer in
  match turn_to_color turn_num with
  | White -> string_of_t (update_time (player_one time))
  | Black -> string_of_t (update_time (player_two time))

let to_string t turn_count : string =
  let board = alive_board t in
  to_string_with_filler_function board
    (fun row_num column_num ->
      if
        not
          (Piece.check_block_covered
             (Pos.pos row_num column_num)
             (get_board board))
      then
        Piece.piece_to_string
          (Piece.piece_at
             (Pos.pos row_num column_num)
             (get_board board))
      else " ")
    turn_count

let rec get_possible_pos (piece : Piece.t) (board : t) : Pos.t list =
  match board with
  | [] -> raise IncorrectId
  | h :: t -> if fst h = piece then snd h else get_possible_pos piece t

let add_piece_to_board (t : t) (pce : Piece.t) : t =
  make_board (pce :: get_board t)

let possible_pos_string t piece : string =
  to_string_with_filler_function t
    (fun row col ->
      if List.mem (Pos.pos row col) (get_possible_pos piece t) then "X"
      else " ")
    ~-1

let empty_board = make_board []
