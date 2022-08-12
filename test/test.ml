(* We decided to go with a strategy for testing that included a lot of
   unit tests. Because we're implementing chess, there are a lot of
   rules that need to be implemented to make sure the game runs
   correctly. This includes movement rules for each piece, global
   movement rules (like not allowing movement into check), checking for
   stalemate, checkmate, and check, etc. All of these rules are testable
   using unit tests, which is what we've done in this document. Most of
   these functions are in the Board and Piece modules, and most of the
   tests were developed using black box testing.

   We can run the tests using [make test]. The end-to-end testing,
   including the intro screen, has been tested manually. We figured this
   was the best course of action for our setup and this project.

   We believe our testing plan demonstrates reasonable correctness of
   our system because chess is so rules-based, that a high number of
   unit tests provides good confidence. *)

open Game
open OUnit2

(* <-----------------------------> *)
(* Some helper pieces *)
let rook1 = Piece.(make_piece Rook (Pos.pos 2 4) 0 White)
let rook2 = Piece.(make_piece Rook (Pos.pos 2 6) 1 White)
let rook3 = Piece.(make_piece Rook (Pos.pos 4 6) 2 Black)
let pawn1 = Piece.(make_piece Pawn (Pos.pos 1 1) 3 White)
let pawn2 = Piece.(make_piece Pawn (Pos.pos 0 2) 4 White)
let pawn3 = Piece.(make_piece Pawn (Pos.pos 2 2) 5 Black)
let pawn4 = Piece.(make_piece Pawn (Pos.pos 0 3) 6 Black)
let pawn5 = Piece.(make_piece Pawn (Pos.pos 6 1) 7 White)
let pawn6 = Piece.(make_piece Pawn (Pos.pos 6 3) 8 Black)

let pawn7 =
  Piece.move_piece
    Piece.(make_piece Pawn (Pos.pos 4 1) 9 White)
    (Pos.pos 4 1)

let pawn8 = Piece.(make_piece Pawn (Pos.pos 0 0) 10 Black)
let pawn9 = Piece.(make_piece Pawn (Pos.pos 0 7) 11 White)
let pawn_a = Piece.(make_piece Pawn (Pos.pos 1 1) 0 White)
let pawn_b = Piece.(make_piece Pawn (Pos.pos 0 2) 1 Black)
let bishop1 = Piece.(make_piece Bishop (Pos.pos 3 3) 12 White)
let bishop2 = Piece.(make_piece Bishop (Pos.pos 1 1) 13 White)
let bishop3 = Piece.(make_piece Bishop (Pos.pos 5 5) 14 Black)
let bishop4 = Piece.(make_piece Bishop (Pos.pos 3 4) 15 Black)
let knight1 = Piece.(make_piece Knight (Pos.pos 3 2) 16 White)
let knight2 = Piece.(make_piece Knight (Pos.pos 5 5) 17 White)
let knight3 = Piece.(make_piece Knight (Pos.pos 7 6) 18 Black)
let knight4 = Piece.(make_piece Knight (Pos.pos 7 4) 19 White)
let knight5 = Piece.(make_piece Knight (Pos.pos 5 6) 20 White)
let knight6 = Piece.(make_piece Knight (Pos.pos 4 6) 21 White)
let knight7 = Piece.(make_piece Knight (Pos.pos 4 5) 22 White)
let queen1 = Piece.(make_piece Queen (Pos.pos 3 3) 23 White)
let queen2 = Piece.(make_piece Queen (Pos.pos 3 1) 24 White)
let queen3 = Piece.(make_piece Queen (Pos.pos 6 6) 25 Black)
let queen4 = Piece.(make_piece Queen (Pos.pos 3 6) 26 Black)
let queen5 = Piece.(make_piece Queen (Pos.pos 1 5) 27 White)
let king1 = Piece.(make_piece King (Pos.pos 3 3) 28 White)
let king2 = Piece.(make_piece King (Pos.pos 7 3) 29 White)
let king3 = Piece.(make_piece King (Pos.pos 7 4) 30 White)
let king4 = Piece.(make_piece King (Pos.pos 7 2) 31 Black)

(* <-----------------------------> *)
(* Some helper boards *)
let rook_board = Board.make_board [ rook1; rook2; rook3 ]

let pawn_board =
  Board.make_board
    [ pawn1; pawn2; pawn3; pawn4; pawn5; pawn6; pawn7; pawn8; pawn9 ]

let pawn_board2 = Board.make_board [ pawn_a; pawn_b ]

let bishop_board =
  Board.make_board [ bishop1; bishop2; bishop3; bishop4 ]

let knight_board =
  Board.make_board
    [ knight1; knight2; knight3; knight4; knight5; knight6; knight7 ]

let queen_board =
  Board.make_board [ queen1; queen2; queen3; queen4; queen5 ]

let king_board = Board.make_board [ king1; king2; king3; king4 ]
let origin = Pos.pos 0 0
let pos1 = Pos.pos 3 6
let pos2 = Pos.pos 4 1
let pos3 = Pos.pos 0 4
let pos4 = Pos.pos 1 0
let pos5 = Pos.pos ~-5 3

(* <-----------------------------> *)
(* Some helper functions *)

(* [test_valid_move name is_valid new_pos piece board] runs a unit test
   named name with Piece.is_valid_move with the following attributed,
   checking whether it's the same as is_valid *)
let test_valid_move name is_valid new_pos piece board =
  name >:: fun _ ->
  assert_equal is_valid
    (Piece.is_valid_move (Board.get_piece_list board) piece new_pos)

(* [test_valid_move name id board flag] runs a unit test named name with
   checking whether id is in board if flag is true, or id is not in
   board if flag is false *)
let test_id_in_game
    (name : string)
    (id : string)
    (board : Board.t)
    (flag : bool) =
  name >:: fun _ ->
  assert_equal flag
    (List.fold_left
       (fun acc piece -> acc || Piece.get_id piece = id)
       false (Board.get_board board))

(* [test_pos name exp_pos test_pos] is a test to check if an exp_pos
   equals test_pos *)
let test_pos (name : string) (exp_pos : Pos.t) (test_pos : Pos.t) : test
    =
  name >:: fun _ -> assert_equal exp_pos test_pos

(* [test_int name exp_int test_int] is a test to check if an exp_int
   equals test_int *)
let test_int (name : string) (exp_int : int) (test_int : int) : test =
  name >:: fun _ -> assert_equal exp_int test_int

(* <-----------------------------> *)
(* Tests *)
let rook_tests =
  [
    test_valid_move "Rook can move one block to the left" true
      (Pos.pos 1 4) rook1 rook_board;
    test_valid_move "Rook can move one block to the right" true
      (Pos.pos 3 4) rook1 rook_board;
    test_valid_move "Rook can move one block up" true (Pos.pos 2 5)
      rook1 rook_board;
    test_valid_move "Rook can move one block down" true (Pos.pos 2 3)
      rook1 rook_board;
    test_valid_move "Rook cannot move diagonal up/left" false
      (Pos.pos 1 5) rook1 rook_board;
    test_valid_move "Rook cannot move diagonal up/right" false
      (Pos.pos 3 5) rook1 rook_board;
    test_valid_move "Rook cannot move diagonal down/left" false
      (Pos.pos 1 3) rook1 rook_board;
    test_valid_move "Rook cannot move diagonal down/right" false
      (Pos.pos 3 3) rook1 rook_board;
    test_valid_move "Rook can move multiple blocks left" true
      (Pos.pos 6 6) rook3 rook_board;
    test_valid_move "Rook can move multiple blocks down" true
      (Pos.pos 2 0) rook1 rook_board;
    test_valid_move "Rook cannot move through same team" false
      (Pos.pos 2 7) rook1 rook_board;
    test_valid_move "Rook cannot move through different team" false
      (Pos.pos 6 6) rook2 rook_board;
    test_valid_move "Rook cannot move onto same team" false
      (Pos.pos 2 6) rook1 rook_board;
    test_valid_move "Rook can move onto different team" true
      (Pos.pos 4 6) rook2 rook_board;
    test_valid_move "Long diagonal doesn't work" false (Pos.pos 1 0)
      rook1 rook_board;
    test_valid_move "Long diagonal doesn't work" false (Pos.pos 0 0)
      rook1 rook_board;
    test_valid_move "Long diagonal doesn't work" false (Pos.pos 6 3)
      rook3 rook_board;
    test_valid_move "Long diagonal doesn't work" false (Pos.pos 6 4)
      rook3 rook_board;
    test_valid_move "Rook cannot move out of bounds up" false
      (Pos.pos 4 9) rook3 rook_board;
    test_valid_move "Rook cannot move out of bounds down" false
      (Pos.pos 2 (-1)) rook1 rook_board;
    test_valid_move "Rook cannot move out of bounds left" false
      (Pos.pos (-1) 4) rook1 rook_board;
    test_valid_move "Rook cannot move out of bounds right" false
      (Pos.pos 8 4) rook1 rook_board;
    test_valid_move "Rook has to move" false (Pos.pos 2 4) rook1
      rook_board;
  ]

let pawn_tests =
  [
    test_valid_move "Unmoved pawn can move forward one" true
      (Pos.pos 1 2) pawn1 pawn_board;
    test_valid_move "Moved pawn can move forward one" true (Pos.pos 4 2)
      pawn7 pawn_board;
    test_valid_move "Unmoved pawn can move forward two" true
      (Pos.pos 1 3) pawn1 pawn_board;
    test_valid_move "Moved pawn cannot move forward two" false
      (Pos.pos 4 3) pawn7 pawn_board;
    test_valid_move "Unmoved pawn cannot move forward three" false
      (Pos.pos 1 4) pawn1 pawn_board;
    test_valid_move "Moved pawn cannot move forward three" false
      (Pos.pos 4 4) pawn7 pawn_board;
    test_valid_move "White pawn cannot move backwards" false
      (Pos.pos 1 0) pawn1 pawn_board;
    test_valid_move "Black pawn cannot move backwards" false
      (Pos.pos 2 3) pawn3 pawn_board;
    test_valid_move "Pawn cannot move diagonal" false (Pos.pos 5 2)
      pawn7 pawn_board;
    test_valid_move "Pawn cannot move diagonal into same team" false
      (Pos.pos 0 2) pawn1 pawn_board;
    test_valid_move "Pawn can move diagonal into different team" true
      (Pos.pos 2 2) pawn1 pawn_board;
    test_valid_move "Pawn can move diagonal into different team" true
      (Pos.pos 0 2) pawn_a pawn_board2;
    test_valid_move "Pawn cannot move into another pawn" false
      (Pos.pos 0 3) pawn2 pawn_board;
    test_valid_move "Pawn cannot move into another pawn" false
      (Pos.pos 0 2) pawn4 pawn_board;
    test_valid_move "Pawn can move in front of pawn" true (Pos.pos 6 2)
      pawn5 pawn_board;
    test_valid_move "Pawn cannot move on pawn two away" false
      (Pos.pos 6 3) pawn5 pawn_board;
    test_valid_move "Black pawn cannot move out of bounds" false
      (Pos.pos 0 (-1)) pawn8 pawn_board;
    test_valid_move "White pawn cannot move out of bounds" false
      (Pos.pos 0 8) pawn8 pawn_board;
    test_valid_move "Pawn has to move" false (Pos.pos 1 1) pawn1
      pawn_board;
  ]

let bishop_tests =
  [
    test_valid_move "Bishop can move one spot to the top right" true
      (Pos.pos 4 4) bishop1 bishop_board;
    test_valid_move "Bishop can move one spot to the top left" true
      (Pos.pos 2 4) bishop1 bishop_board;
    test_valid_move "Bishop can move one spot to the bottom right" true
      (Pos.pos 4 2) bishop1 bishop_board;
    test_valid_move "Bishop can move one spot to the bottom left" true
      (Pos.pos 2 2) bishop1 bishop_board;
    test_valid_move "Random bishop valid movement" true (Pos.pos 6 1)
      bishop4 bishop_board;
    test_valid_move "Random bishop valid movement" true (Pos.pos 5 2)
      bishop4 bishop_board;
    test_valid_move "Random bishop valid movement" true (Pos.pos 5 6)
      bishop4 bishop_board;
    test_valid_move "Random bishop valid movement" true (Pos.pos 6 1)
      bishop4 bishop_board;
    test_valid_move "Random non-diagonal moves 1" false (Pos.pos 4 6)
      bishop4 bishop_board;
    test_valid_move "Random non-diagonal moves 2" false (Pos.pos 4 6)
      bishop4 bishop_board;
    test_valid_move "Random non-diagonal moves 3" false (Pos.pos 4 6)
      bishop4 bishop_board;
    test_valid_move "Random non-diagonal moves 4" false (Pos.pos 2 2)
      bishop4 bishop_board;
    test_valid_move "Random non-diagonal moves 5" false (Pos.pos 0 6)
      bishop4 bishop_board;
    test_valid_move "Bishop cannot move through same team" false
      (Pos.pos 0 0) bishop1 bishop_board;
    test_valid_move "Bishop cannot move through different team" false
      (Pos.pos 6 6) bishop1 bishop_board;
    test_valid_move "Bishop cannot move on same team" false
      (Pos.pos 1 1) bishop1 bishop_board;
    test_valid_move "Bishop can move on different team" true
      (Pos.pos 5 5) bishop1 bishop_board;
    test_valid_move "Bishop cannot move out of bounds" false
      (Pos.pos (-1) 7) bishop1 bishop_board;
    test_valid_move "Bishop cannot move out of bounds" false
      (Pos.pos 7 (-1)) bishop1 bishop_board;
    test_valid_move "Bishop cannot move out of bounds" false
      (Pos.pos 2 8) bishop3 bishop_board;
    test_valid_move "Bishop cannot move out of bounds" false
      (Pos.pos 8 2) bishop3 bishop_board;
    test_valid_move "Bishop must move" false (Pos.pos 3 3) bishop1
      bishop_board;
  ]

let knight_tests =
  [
    test_valid_move "Knight can move two up one right" true
      (Pos.pos 4 4) knight1 knight_board;
    test_valid_move "Knight can move one up two right" true
      (Pos.pos 5 3) knight1 knight_board;
    test_valid_move "Knight can move one up two left" true (Pos.pos 1 3)
      knight1 knight_board;
    test_valid_move "Knight can move two up one left" true (Pos.pos 2 4)
      knight1 knight_board;
    test_valid_move "Knight can move two down one right" true
      (Pos.pos 4 0) knight1 knight_board;
    test_valid_move "Knight can move one down two right" true
      (Pos.pos 5 1) knight1 knight_board;
    test_valid_move "Knight can move one down two left" true
      (Pos.pos 1 1) knight1 knight_board;
    test_valid_move "Knight can move two down one left" true
      (Pos.pos 2 0) knight1 knight_board;
    test_valid_move "Knight cannot move onto same team" false
      (Pos.pos 7 4) knight2 knight_board;
    test_valid_move "Knight can move onto different team" true
      (Pos.pos 7 6) knight2 knight_board;
    test_valid_move "Random invalid movements" false (Pos.pos 3 5)
      knight1 knight_board;
    test_valid_move "Random invalid movements" false (Pos.pos 2 3)
      knight1 knight_board;
    test_valid_move "Random invalid movements" false (Pos.pos 4 2)
      knight1 knight_board;
    test_valid_move "Random invalid movements" false (Pos.pos 2 5)
      knight1 knight_board;
    test_valid_move "Random invalid movements" false (Pos.pos 1 6)
      knight1 knight_board;
    test_valid_move "Knight can jump over pieces" true (Pos.pos 3 6)
      knight2 knight_board;
    test_valid_move "Knight cannot move out of bounds" false
      (Pos.pos 9 7) knight3 knight_board;
    test_valid_move "Knight cannot move out of bounds" false
      (Pos.pos 8 2) knight4 knight_board;
    test_valid_move "Knight must move" false (Pos.pos 3 2) knight1
      knight_board;
  ]

let queen_tests =
  [
    test_valid_move "Queen can move one block to the left" true
      (Pos.pos 2 3) queen1 queen_board;
    test_valid_move "Queen can move one block to the right" true
      (Pos.pos 4 3) queen1 queen_board;
    test_valid_move "Queen can move one block up" true (Pos.pos 3 4)
      queen1 queen_board;
    test_valid_move "Queen can move one block down" true (Pos.pos 3 2)
      queen1 queen_board;
    test_valid_move "Queen can move up and to the right" true
      (Pos.pos 4 4) queen1 queen_board;
    test_valid_move "Queen can move down and to the right" true
      (Pos.pos 4 2) queen1 queen_board;
    test_valid_move "Queen can move up and to the left" true
      (Pos.pos 2 4) queen1 queen_board;
    test_valid_move "Queen can move down and to the left" true
      (Pos.pos 2 2) queen1 queen_board;
    test_valid_move "Queen cannot move through same team" false
      (Pos.pos 3 0) queen1 queen_board;
    test_valid_move "Queen cannot move through different team" false
      (Pos.pos 3 7) queen1 queen_board;
    test_valid_move "Queen cannot move on to same team" false
      (Pos.pos 3 1) queen1 queen_board;
    test_valid_move "Queen can move on to different team" true
      (Pos.pos 3 6) queen1 queen_board;
    test_valid_move "Random valid movements" true (Pos.pos 5 5) queen1
      queen_board;
    test_valid_move "Random valid movements" true (Pos.pos 3 5) queen1
      queen_board;
    test_valid_move "Random valid movements" true (Pos.pos 0 0) queen1
      queen_board;
    test_valid_move "Random valid movements" true (Pos.pos 0 3) queen1
      queen_board;
    test_valid_move "Random invalid movements" false (Pos.pos 0 4)
      queen1 queen_board;
    test_valid_move "Random invalid movements" false (Pos.pos 4 5)
      queen1 queen_board;
    test_valid_move "Random invalid movements" false (Pos.pos 7 5)
      queen1 queen_board;
    test_valid_move "Queen cannot move out of bounds" false
      (Pos.pos 8 3) queen1 queen_board;
    test_valid_move "Queen must move" false (Pos.pos 3 3) queen1
      queen_board;
  ]

let king_tests =
  [
    test_valid_move "King can move one block to the left" true
      (Pos.pos 2 3) king1 king_board;
    test_valid_move "King can move one block to the right" true
      (Pos.pos 4 3) king1 king_board;
    test_valid_move "King can move one block up" true (Pos.pos 3 4)
      king1 king_board;
    test_valid_move "King can move one block down" true (Pos.pos 3 2)
      king1 king_board;
    test_valid_move "King can move up and to the right" true
      (Pos.pos 4 4) king1 king_board;
    test_valid_move "King can move down and to the right" true
      (Pos.pos 4 2) king1 king_board;
    test_valid_move "King can move up and to the left" true
      (Pos.pos 2 4) king1 king_board;
    test_valid_move "King can move down and to the left" true
      (Pos.pos 2 2) king1 king_board;
    test_valid_move "King cannot move more than one block" false
      (Pos.pos 3 5) king1 king_board;
    test_valid_move "King cannot move more than one block" false
      (Pos.pos 5 3) king1 king_board;
    test_valid_move "King cannot move more than one block" false
      (Pos.pos 5 5) king1 king_board;
    test_valid_move "King cannot move more than one block" false
      (Pos.pos 5 4) king1 king_board;
    test_valid_move "King cannot move more than one block" false
      (Pos.pos 3 1) king1 king_board;
    test_valid_move "King cannot move into same team" false
      (Pos.pos 7 4) king2 king_board;
    test_valid_move "King can move into different team" true
      (Pos.pos 7 2) king2 king_board;
    test_valid_move "King cannot move out of bounds" false (Pos.pos 8 3)
      king2 king_board;
    test_valid_move "King must move" false (Pos.pos 3 3) king1
      king_board;
  ]

let lwrook = Piece.make_piece Piece.Rook (Pos.pos 0 0) 0 Piece.White
let rwrook = Piece.make_piece Piece.Rook (Pos.pos 7 0) 1 Piece.White
let lbrook = Piece.make_piece Piece.Rook (Pos.pos 0 7) 0 Piece.Black
let rbrook = Piece.make_piece Piece.Rook (Pos.pos 7 7) 1 Piece.Black
let wking = Piece.make_piece Piece.King (Pos.pos 4 0) 0 Piece.White
let bking = Piece.make_piece Piece.King (Pos.pos 4 7) 0 Piece.Black
let moved_lw_rook = Piece.move_piece lwrook (Pos.pos 0 1)
let dir_threat_rb_rook = Piece.move_piece rbrook (Pos.pos 4 6)
let indir_threat_lb_rook = Piece.move_piece lbrook (Pos.pos 3 5)

let reg_board =
  Board.make_board [ lwrook; rwrook; lbrook; rbrook; wking; bking ]

let invalid_board =
  Board.make_board
    [
      moved_lw_rook;
      lwrook;
      rwrook;
      indir_threat_lb_rook;
      dir_threat_rb_rook;
      bking;
    ]

let test_castle name expected is_king_side king rook board =
  name >:: fun _ ->
  assert_equal expected
    (Piece.is_valid_castle
       (Board.get_piece_list board)
       king rook is_king_side)

let castle_tests =
  [
    test_castle "white longside" true false wking lwrook reg_board;
    test_castle "black longside" true false bking lbrook reg_board;
    test_castle "white shortside" true true wking rwrook reg_board;
    test_castle "black shortside" true true bking rbrook reg_board;
    test_castle "can't when rook in wrong place" false false wking
      moved_lw_rook invalid_board;
  ]

let captured_board =
  Board.update queen_board (Pos.pos 3 3) (Piece.get_id queen4)

let capture_tests =
  [
    test_id_in_game "Black queen starts in game" (Piece.get_id queen4)
      queen_board true;
    test_id_in_game "White queen starts in game" (Piece.get_id queen1)
      queen_board true;
    test_id_in_game "Black queen stays in game after capturing"
      (Piece.get_id queen4) captured_board true;
    test_id_in_game "White queen leaves game after capturing"
      (Piece.get_id queen1) captured_board false;
  ]

let pos_tests =
  [
    test_int "Pos first is 0 for origin" 0 (Pos.fst origin);
    test_int "Pos second is 0 for origin" 0 (Pos.snd origin);
    test_int "Pos first works for arbitrary pos" 3 (Pos.fst pos1);
    test_int "Pos first works for arbitrary pos" 4 (Pos.fst pos2);
    test_int "Pos second works for arbitrary pos" 6 (Pos.snd pos1);
    test_int "Pos second works for arbitrary pos" 1 (Pos.snd pos2);
    test_pos "Pos add works correctly" (Pos.pos 7 7) (Pos.add pos1 pos2);
    test_pos "Pos minus works correctly" (Pos.pos ~-1 5)
      (Pos.minus pos1 pos2);
    test_pos "Pos multiplication works correctly" (Pos.pos 9 18)
      (Pos.multiply pos1 3);
    test_int "Pos non-zero with origin is 0" 0
      (Pos.non_zero_entry origin);
    test_int "Pos non-zero with first element 0" 4
      (Pos.non_zero_entry pos3);
    test_int "Pos non-zero with second element 0" 1
      (Pos.non_zero_entry pos4);
    test_pos "Make ones works correctly" (Pos.pos ~-1 1)
      (Pos.make_ones pos5);
    test_pos "Make ones is identity on origin" (Pos.pos 0 0)
      (Pos.make_ones origin);
  ]

let suite =
  "test suite for chess"
  >::: List.flatten
         [
           rook_tests;
           pawn_tests;
           bishop_tests;
           knight_tests;
           queen_tests;
           king_tests;
           castle_tests;
           capture_tests;
           pos_tests;
         ]

let _ = run_test_tt_main suite