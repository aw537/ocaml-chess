val parse_basic_move : string -> Piece.t list -> Pos.t * string
(** parse_command input returns a pair of a position and piece id
    Precondition: input is of the form "TeamPieceNumber:LetterNumber" *)

(* val string_to_id: string -> int *)

val run_game : unit -> int
