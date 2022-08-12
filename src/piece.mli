exception InvalidMove
exception EmptySpace

open Pos

type t
(** The type to represent a piece and its data *)

(** The type to represent the role of a piece *)
type piece_type =
  | Pawn
  | Rook
  | Knight
  | Bishop
  | King
  | Queen

(** The type to represent the color of a piece *)
type color =
  | White
  | Black

val make_piece : piece_type -> Pos.t -> int -> color -> t
(** [make_piece role pos id_number team] is a piece with the given
    attributes *)

val get_pos : t -> Pos.t
(** [get_pos piece] is the position of piece *)

val is_alive : t -> bool
(** [is_alive piece] is whether or not piece is still alive *)

val get_id : t -> string
(** [get_id piece] is a piece's id *)

val get_team : t -> color
(** [get_team piece] is the team [piece] belongs to *)

val get_role : t -> piece_type
(** [get_role piece] is the type of chess piece that [piece] is *)

val is_valid_move : t list -> t -> Pos.t -> bool
(** [update_pos piece_list piece new_loc] is a true if the new_loc is
    reachable by piece, given the current piece_list of pieces on the
    board, and false otherwise Note: Does not check whether a piece is
    moving into check *)

val is_valid_castle : t list -> t -> t -> bool -> bool
(** [is_valid_castle piece_list k r king_side] is true if king [k] can
    castle with rook [r], and is false otherwise.*)

val can_be_promoted : t -> bool
(** [can_be_promoted p] is true if piece [p] is a pawn, and on the 7th
    or 0th rank for White and Black pawns respectively, and alive. False
    otherwise*)

val promote : t -> piece_type -> int -> t
(** [promote p r n] is piece [p] with role [r] and id from [p]'s team,
    the new role, and the number [n].*)

val move_piece : t -> Pos.t -> t
(** [move_piece piece new_pos] is piece that's been moved to new_pos *)

val piece_at : Pos.t -> t list -> t
(** [piece_at position board] returns the piece at a specified position
    on a board, raises an ExmptySpace exception if there is no piece on
    that position *)

val piece_to_string : t -> string
(** [piece_to_string piece] returns the string representation of a piece
    depending on the piece's role and team *)

val check_block_covered : Pos.t -> t list -> bool
(** [check_block_covered] is true if block at pos is unoccupied in
    curr_board and false otherwise *)

val kill : t -> t
(** [kill piece] is piece but dead *)

val command_format : Pos.t -> t list -> string
(** [command_format p b] is the command notation of [p] on [b] e.g.,
    white pawn at (4,1) with id 1 is represented as "WP1"*)

val num_role : t list -> piece_type -> color -> int
(** [num_role pces r c] is the number of pieces in [pces] that are of
    role [r] and belong to team [c] *)

val get_points : t -> int
(** [get_points p] is a getter for the point value of [p] *)

val pts_at_pos : Pos.t -> t list -> int
(** [num_role b r c] is the number of pieces that have role [r] on team
    [c] in [b] *)
