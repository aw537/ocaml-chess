exception InvalidRole of string

type t
(** The type to represent a board, a list of pieces *)

val make_board : Piece.t list -> t
(** Make a board with the given pieces *)

val get_board_with_moves : t -> (Piece.t * Pos.t list) list
(** [get_board_with_moves board] is a list of pieces with their possible
    moves. Does not include moves that move into check *)

val get_piece_list : t -> Piece.t list
(** [get_piece_pos_list board] is the list of positions inhabited by
    alive pieces in board *)

val needs_promote : t -> Piece.t option
(** [needs_promote b] is [Some p] that needs to be promoted, or [None]
    otherwise.*)

val update : t -> Pos.t -> string -> t
(** [update board new_pos id] is the new board after moving piece with
    the id to new_pos *)

val castle : t -> Piece.t -> Piece.t -> bool -> t
(** [castle board king rook king_side] is the new board after castling
    the king on queenside or kingside, depending on [king_side]. *)

val is_check : t -> int -> bool
(** [is_check board turn_count] is whether or not the current turn's
    team is checked on [board] Precondition: that team must have exactly
    one king *)

val is_checkmate : t -> int -> bool
(** [is_checkmate board turn_count] is whether or not the board is
    checked-mated for the team on turn_count *)

val is_stalemate : t -> int -> bool
(** [is_stalemate board turn_count] is whether or not the board is
    stale-mated for the team on turn_count *)

val get_possible_pos : Piece.t -> t -> Pos.t list
(** [get_possible_pos piece board] is a list of valid positions that
    piece can move to *)

val to_string : t -> int -> string
(** [to_string board turncount] is a string representation of board *)

val possible_pos_string : t -> Piece.t -> string
(** [to_string board piece] is a string representation of the possible
    locations piece can move to on board *)

val id_to_piece : t -> string -> Piece.t
(** [id_to_piece board id] is the piece referenced by id *)

val promote : t -> Piece.t -> string -> t
(** [promote b p s] is [b] with [p] promoted to the role represented by
    [s] *)

val get_board : t -> Piece.t list
(** [get_board board] is the list of pieces making board *)

val add_piece_to_board : t -> Piece.t -> t

val empty_board : t
(** [empty_board] is a representation of a board with no pieces *)

val turn_to_color : int -> Piece.color
(** [turn_to_color turn_num] is either Black or White, depending on the
    team *)

val timer_string : int -> string

val turn_to_color_string : int -> string
(** [turn_to_color_string turn] is the team playing on turn *)