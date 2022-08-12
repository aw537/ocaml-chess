exception UnImplemented
exception InvalidMovement
exception InvalidCommand

let string_to_pos p_string =
  let file = p_string.[0] |> Char.code |> ( + ) ~-97 in
  let rank = p_string.[1] |> int_of_char |> ( + ) ~-49 in
  Pos.pos file rank

let parse_basic_move command brd =
  let command_parts =
    command |> String.trim
    |> String.split_on_char ':'
    |> List.filter (fun s -> s <> "")
  in
  match command_parts with
  | [ piece; pos ] -> (
      ( string_to_pos pos,
        match Piece.command_format (string_to_pos piece) brd with
        | exception Piece.EmptySpace -> ""
        | p_id -> p_id ))
  | _ -> failwith "Command not formatted correctly"

(** [verify_turn piece_id turn_count] is true if the correct team is
    moving on a specified turn; White team moves first, then black, then
    white, etc e.g. if turn_count is 1 and the piece moving is white,
    [verify_turn] is true if turn_count is 4 and piece moving is black,
    [verify_turn] is true if turn_count is 27 and piece moving is black,
    [verify_turn] is false*)
let verify_turn piece_id turn_count =
  let color_moving = String.get piece_id 0 in
  match color_moving with
  | 'W' -> if turn_count mod 2 = 1 then true else false
  | 'B' -> if turn_count mod 2 = 0 then true else false
  | _ -> false

(* [run_command new_pos id board] is Some new board if the movement was
   valid and None otherwise *)
let run_command new_pos id board turn_count =
  if id = "" then None
  else if
    Piece.is_valid_move
      (Board.get_piece_list board)
      (Board.id_to_piece board id)
      new_pos
    && verify_turn id turn_count
  then Some (Board.update board new_pos id)
  else None

let run_castle (board : Board.t) (turn_count : int) (king_side : bool) =
  let team =
    if turn_count mod 2 = 0 then Piece.Black else Piece.White
  in
  let team_char = if team = Piece.White then "W" else "B" in
  let king_id = team_char ^ "K0" in
  let rook_id = team_char ^ "R" ^ if king_side then "1" else "0" in
  let king = Board.id_to_piece board king_id in
  let rook = Board.id_to_piece board rook_id in
  if
    Piece.is_valid_castle
      (Board.get_piece_list board)
      king rook king_side
    && not
         (* The square the king crosses through may not be
            threatened. *)
         (Board.is_check
            (Board.update board
               (0
               |> Pos.pos (if king_side then 1 else -1)
               |> Pos.add (Piece.get_pos king))
               king_id)
            turn_count)
  then Some (Board.castle board king rook king_side)
  else None

let separate_action command =
  let command_parts =
    command |> String.trim
    |> String.split_on_char ' '
    |> List.filter (fun s -> s <> "")
  in
  match command_parts with
  | [] -> raise InvalidCommand
  | [ verb ] -> if verb = "quit" then `Quit else raise InvalidCommand
  | verb :: [ vobject ] -> (
      match verb with
      | "show" -> `Show vobject
      | "castle" -> `Castle vobject
      | "move" -> `Move vobject
      | _ -> raise InvalidCommand)
  | _ -> raise InvalidCommand

(* [append_strings_horizontal s1 s2] is s1 and s2 printed horizontally
   along each other*)
let append_strings_horizontal (s1 : string) (s2 : string) =
  let split_fun = String.split_on_char '\n' in
  let s1_split = split_fun s1 in
  let s2_split = split_fun s2 in
  let rec append_helper s1_list s2_list =
    "\n"
    ^
    match (s1_list, s2_list) with
    | [], [] -> ""
    | h :: t, [] | [], h :: t -> h ^ append_helper t []
    | h1 :: t1, h2 :: t2 -> h1 ^ "   " ^ h2 ^ append_helper t1 t2
  in
  append_helper s1_split s2_split

(* Run a promotion for piece on board *)
let rec do_promotion_repl (board : Board.t) (piece : Piece.t) =
  print_endline
    "The game has detected you moved a pawn all the way to the other \
     side of the board. \n\
    \ It must be promoted.";
  print_endline
    "Enter a role to promote to. (N = Knight, B = Bishop, R = Rook, Q \
     = Queen)";
  print_string "> ";
  match read_line () with
  | exception End_of_file ->
      failwith "This shouldn't have happened. See do_promotion_repl"
  | role ->
      let new_board =
        try Board.promote board piece role
        with Board.InvalidRole s ->
          print_endline
            (s ^ " was not a valid role input. Please try again.");
          do_promotion_repl board piece
      in
      new_board

(* [cmd_role c] is the piece role represented by a new piece format
   E.g., BKe5 is Piece.King *)
let cmd_role cmd =
  match String.get cmd 1 with
  | 'P' -> Piece.Pawn
  | 'R' -> Piece.Rook
  | 'N' -> Piece.Knight
  | 'B' -> Piece.Bishop
  | 'K' -> Piece.King
  | 'Q' -> Piece.Queen
  | _ -> Piece.Pawn

(* [cmd_team c] is the piece color represented by a new piece format
   E.g., BKe5 is Piece.Black *)
let cmd_team cmd =
  match String.get cmd 0 with
  | 'B' -> Piece.Black
  | 'W' -> Piece.White
  | _ -> Piece.Black

(* Takes the original format and returns the piece it represents E.g.,
   WPa1 -> a white pawn with a position at (0, 0) *)
let parse_piece_command pce_cmd id =
  let team = cmd_team pce_cmd in
  let role = cmd_role pce_cmd in
  let rank = String.get pce_cmd 2 in
  let file = String.get pce_cmd 3 in
  Piece.make_piece role
    (string_to_pos (Char.escaped rank ^ Char.escaped file))
    id team

let run_turn_show curr_board turn_count vobject =
  try
    let id =
      Piece.command_format (string_to_pos vobject)
        (Board.get_board curr_board)
    in
    Some (curr_board, Some (Board.id_to_piece curr_board id), turn_count)
  with e ->
    print_endline "I'm sorry, that is not a valid piece.";
    Some (curr_board, None, turn_count)

(* Run a turn for the castle command *)
let run_turn_castle curr_board turn_count is_checked vobject =
  if is_checked then (
    print_endline "You cannot castle while in check. Please try again.";
    Some (curr_board, None, turn_count))
  else
    let king_side =
      match vobject with
      | "king" -> true
      | "kingside" -> true
      | "short" -> true
      | "queen" -> false
      | "queenside" -> false
      | "long" -> false
      | _ -> raise InvalidCommand
    in
    match run_castle curr_board turn_count king_side with
    | None ->
        print_endline
          "To castle, your king and the rook in question cannot have \
           moved already. The king may not cross through any squares \
           that are threatened. And, all the spaces between the rook \
           and king must be clear. Please try again.";
        Some (curr_board, None, turn_count)
    | Some b -> Some (b, None, turn_count + 1)

(* Check if a board is not moving into check and the move is valid *)
let make_no_prom_move curr_board turn_count new_pos id =
  match run_command new_pos id curr_board turn_count with
  | None ->
      print_endline "That was not a valid move. Please try again.";
      curr_board
  | Some b ->
      if Board.is_check b turn_count then (
        print_endline
          "You cannot remain in check after moving. Please try again.";
        curr_board)
      else b

(* Run a turn for the move command *)
let run_turn_move curr_board turn_count verb =
  try
    let new_pos, id =
      parse_basic_move verb (Board.get_board curr_board)
    in
    let no_prom_new_board =
      make_no_prom_move curr_board turn_count new_pos id
    in
    let new_board =
      match Board.needs_promote no_prom_new_board with
      | None -> no_prom_new_board
      | Some piece -> do_promotion_repl no_prom_new_board piece
    in
    Some
      ( new_board,
        None,
        if new_board = curr_board then turn_count else turn_count + 1 )
  with e ->
    (match e with
    | Piece.EmptySpace -> print_endline "That's an empty space."
    | _ ->
        print_endline "I'm sorry, that command was not well-formatted.");
    Some (curr_board, None, turn_count)

(* Runs a turn assuming the game isn't over with a split command *)
let run_turn_w_command
    curr_board
    piece_show
    turn_count
    is_checked
    command =
  try
    let command_split = separate_action command in
    match command_split with
    | `Quit -> None
    | `Move verb -> run_turn_move curr_board turn_count verb
    | `Castle vobject ->
        run_turn_castle curr_board turn_count is_checked vobject
    | `Show vobject -> run_turn_show curr_board turn_count vobject
  with e ->
    print_endline "I'm sorry, that command was not well-formatted.";
    Some (curr_board, None, turn_count)

(* [run_turn curr_board piece_show turn_count] is a helper function for
   run_repl, and it runs one turn of the game assuming the game is not
   over. Returns none if the game should end and the parameters for the
   next repl run if not *)
let run_turn
    (curr_board : Board.t)
    (piece_show : Piece.t option)
    (turn_count : int) : (Board.t * Piece.t option * int) option =
  let is_checked = Board.is_check curr_board turn_count in
  print_endline ("Is checked: " ^ if is_checked then "Yes" else "No");
  print_endline "Enter your command";
  print_string "> ";
  match read_line () with
  | exception End_of_file -> None
  | command ->
      run_turn_w_command curr_board piece_show turn_count is_checked
        command

(* [play_again] asks the user to input whether they want to play another
   game of Chess. 0 indicates that the user wishes to quit, while 1
   indicates that the user wishes to play another game *)
let rec play_again () : int =
  print_endline "Would you like to play another game?";
  print_string "> ";
  let check =
    match read_line () with
    | exception End_of_file -> exit 0
    | command ->
        if command = "play" || command = "yes" then 1
        else if command = "quit" || command = "no" then 0
        else 2
  in
  match check with
  | 0 -> exit 0
  | 2 ->
      print_endline "Sorry, I didn't understand that.\n";
      play_again ()
  | a -> a

(* Prints all the info needed for a turn, like board, and returns true
   if the turn needs to be ran and false otherwise. Also returns whether
   or not to play again *)
let print_board_info curr_board piece_show turn_count : int * bool =
  let show_string =
    match piece_show with
    | None -> ""
    | Some piece -> "\n\n" ^ Board.possible_pos_string curr_board piece
  in
  print_string
    (append_strings_horizontal
       (Board.to_string curr_board turn_count)
       show_string);
  print_string (Board.timer_string turn_count);
  print_endline "";
  if Board.is_checkmate curr_board turn_count then (
    print_endline
      (Board.turn_to_color_string turn_count ^ " has been checkmated.\n");
    (play_again (), false))
  else if Board.is_stalemate curr_board turn_count then (
    print_endline "Stalemate.\n";
    (play_again (), false))
  else (0, true)

(* [run_repl curr_board piece_show turn_count black_fun] runs the repl
   loop for the game. curr_board is the current board. piece_show is an
   option. None if we don't want to show possible moves, and Some piece
   if we want to show the piece's possible moves. To show possible
   pieces, type "show <id>", where <id> is in the format
   TeamPieceNumber, uppercase. black_fun and white_fin are options. It
   indicates the function to choose a piece for it's respective team, or
   none if a human plays. *)
let rec run_repl
    (curr_board : Board.t)
    (piece_show : Piece.t option)
    (turn_count : int)
    (white_fun : (Board.t -> Piece.color -> Board.t) option)
    (black_fun : (Board.t -> Piece.color -> Board.t) option) : int =
  let p_again, turn_flag =
    print_board_info curr_board piece_show turn_count
  in
  if turn_flag then
    match (white_fun, black_fun, Board.turn_to_color turn_count) with
    | None, _, White | _, None, Black -> (
        match run_turn curr_board piece_show turn_count with
        | None -> 0
        | Some (new_board, new_show, new_count) ->
            run_repl new_board new_show new_count white_fun black_fun)
    | Some ai_fun, _, White ->
        run_repl
          (ai_fun curr_board White)
          None (turn_count + 1) white_fun black_fun
    | _, Some ai_fun, Black ->
        run_repl
          (ai_fun curr_board Black)
          None (turn_count + 1) white_fun black_fun
  else p_again

(** [make_a_piece b c i] is a new board made from taking existing board
    [b], and adding a piece represented in a string format [c] *)
let rec make_a_piece brd cmd id =
  match cmd with
  | piece -> Board.add_piece_to_board brd (parse_piece_command piece id)

(** [run_make_a_board b wp bp] is the loop for the make-a-piece mode
    that keeps prompting the player to input a piece in a specific
    format, to return a customized board until the player wants to begin
    playing with the 'play' command. Both teams must be given a king,
    and no pawns can begin at the opposite ends of the board where they
    would be promoted. Note: The input from the player to create a piece
    must be in the format 'team_color' ^ 'piece_role' ^ position in
    standard chess notation E.g., BPa4 returns a board with a black pawn
    at a4, assuming that place is not taken *)
let rec run_make_a_board brd : Board.t =
  print_endline "";
  print_endline (Board.to_string brd (-1));
  print_endline
    "Create a piece (each team must have one king); no duplicate \
     pieces on one position allowed";
  print_string "> ";
  match read_line () with
  | "play" -> brd
  | cmd ->
      let role = cmd_role cmd in
      let team = cmd_team cmd in
      let id = Piece.num_role (Board.get_board brd) role team in
      if String.get cmd 0 = 'W' then
        run_make_a_board (make_a_piece brd cmd id)
      else run_make_a_board (make_a_piece brd cmd id)

let string_to_path str = "data" ^ Filename.dir_sep ^ str ^ ".json"

let json_to_piece json =
  let get_field name f j = j |> Yojson.Basic.Util.member name |> f in
  let xPos = json |> get_field "xPos" Yojson.Basic.Util.to_int in
  let yPos = json |> get_field "yPos" Yojson.Basic.Util.to_int in
  let id = json |> get_field "id_num" Yojson.Basic.Util.to_int in
  let role_s = json |> get_field "class" Yojson.Basic.Util.to_string in
  let role =
    match role_s with
    | "pawn" -> Piece.(Pawn)
    | "rook" -> Piece.(Rook)
    | "bishop" -> Piece.(Bishop)
    | "knight" -> Piece.(Knight)
    | "queen" -> Piece.(Queen)
    | "king" -> Piece.(King)
    | _ -> failwith (role_s ^ " is not a valid role")
  in
  let team_s = json |> get_field "team" Yojson.Basic.Util.to_string in
  let team =
    match team_s with
    | "white" -> Piece.(White)
    | "black" -> Piece.(Black)
    | _ -> failwith (team_s ^ " is not a valid team")
  in
  Piece.make_piece role (Pos.pos xPos yPos) id team

(* [file_to_board str] is the board referenced by str *)
let file_to_board str =
  let pieces =
    str |> Yojson.Basic.from_file
    |> Yojson.Basic.Util.member "pieces"
    |> Yojson.Basic.Util.to_list |> List.map json_to_piece
  in
  Board.make_board pieces

(* Print the relevant info for the player to choose their board *)
let print_get_board_text () : unit =
  print_endline "What board would you like to use?";
  print_endline
    "Included boards: default, check, checkmate, stalemate, pawn_test";
  print_endline
    "If you've placed a valid board .json file in the data folder, you \
     can enter its name too.";
  print_endline
    "Or, enter \"make board\" to create your own board right now.";
  print_string "> "

let rec get_board () : Board.t option =
  print_get_board_text ();
  match read_line () with
  | exception End_of_file -> None
  | command -> (
      if command = "quit" then exit 0
      else if command = "make board" then
        Some (run_make_a_board Board.empty_board)
      else
        match command |> string_to_path |> file_to_board with
        | exception _ ->
            print_endline
              "Something went wrong with that board. Make sure your \
               file is in the data folder, and you're not including \
               the file extension, or any parent folders.";
            get_board ()
        | board -> Some board)

(* [text_to_fun_ai ai_fun] is the function that runs ai_fun. Defaults to
   human. *)
let text_to_fun_ai (ai_fun : string) :
    (Board.t -> Piece.color -> Board.t) option =
  match ai_fun with
  | "basic" -> Some Ai.basic
  | "random" -> Some Ai.random
  | "smart" -> Some Ai.smart
  | "human" -> None
  | _ -> None

(* [get_intro_text] prints out the intro text of the game and returns
   the applicable information *)
let get_intro_text () =
  let player_string = "There is: human, smart, basic, random" in
  print_endline "\nWelcome to Chess! Who would you like white to play?";
  print_endline player_string;
  print_string "> ";
  let white_player = text_to_fun_ai (read_line ()) in
  print_endline "Who would you like black to play?";
  print_endline player_string;
  print_string "> ";
  let black_player = text_to_fun_ai (read_line ()) in
  (white_player, black_player)

(* Run the game engine *)
let rec run_game () =
  let white_player, black_player = get_intro_text () in
  let result =
    match (white_player, black_player) with
    | None, None -> (
        match get_board () with
        | None -> 0
        | Some board -> run_repl board None 1 None None)
    | white_mv, black_mv ->
        run_repl
          (file_to_board (string_to_path "default"))
          None 1 white_mv black_mv
  in
  match result with
  | 0 -> exit 0
  | _ -> run_game ()
