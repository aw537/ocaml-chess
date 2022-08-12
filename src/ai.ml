exception NoMoves

type board_rep = (Piece.t * Pos.t list) list

(* board_rep, representing a move, paired with an int representing points
   to be gained from that move *)
type pos_pts = (Piece.t * Pos.t * int) list

(* returns the highest amount of points to be gained given pos_pts *)
let most_points pos_pts team = 
  let rec iterator team p = function
  | [] -> p 
  | (_, _, n) :: t when team = Piece.Black -> 
    if n > p then iterator team n t
    else iterator team p t
  | (_, _, n) :: t -> 
    if n < p then iterator team n t
    else iterator team p t
  in iterator team 0 pos_pts

let rec find_pts board_rep board : (Piece.t * Pos.t * int) list =
  match board_rep with
  | [] -> []
  | (pce, moves) :: t ->
    match moves with
    | [] -> find_pts t board
    | move :: t2 -> (pce, move, Piece.pts_at_pos move board) :: find_pts ((pce, t2) :: t) board

let rec get_smart_move best_num = function
  | [] -> failwith "impossible"
  | (a, b, c) :: t -> if c = best_num then (a, b) else get_smart_move best_num t

let team_only (team : Piece.color) (board_pieces : board_rep) : board_rep = 
  List.filter (fun piece -> Piece.(get_team (fst piece) = team)) board_pieces

let basic board team =
  let rec move_search pieces = 
    match pieces with
    | (_, []) :: t -> move_search t
    | (piece, pos_move :: _) :: _ ->
      Board.update board pos_move (Piece.get_id piece)
    | [] -> raise NoMoves
  in move_search (board |> Board.get_board_with_moves |> team_only team) 

let random board team = 
  let piece_moves = board |> Board.get_board_with_moves |> team_only team in
  let valid_piece_moves = List.filter
    (fun piece -> List.length (snd piece) > 0)
    piece_moves
  in
  let piece_idx = Random.int (List.length valid_piece_moves) in
  let piece = List.nth valid_piece_moves piece_idx in
  let pos_idx = Random.int (List.length (snd piece)) in
  Board.update board (List.nth (snd piece) pos_idx) (Piece.get_id (fst piece))

let smart (board:Board.t) (team: Piece.color) = 
  let piece_moves = board |> Board.get_board_with_moves |> team_only team in
  let valid_piece_moves = List.filter
    (fun piece -> List.length (snd piece) > 0)
    piece_moves
in
let max = most_points (find_pts valid_piece_moves (Board.get_board board)) team 
in 
match get_smart_move max (find_pts valid_piece_moves (Board.get_board board)) with
| (pce, pos) -> Board.update board pos (Piece.get_id pce)

  
    
