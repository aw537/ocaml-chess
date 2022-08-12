exception NoMoves

val basic : Board.t -> Piece.color -> Board.t
(** [basic board team] is the new board after an ai makes a move for
team randomly. This is guaranteed to only make an allowed move.
Precondition: There must be moves available for team. *)

val random: Board.t -> Piece.color -> Board.t
(** [random board team] is the new board after a very basic ai makes a move for
team by choosing the first available move. This is guaranteed to only
make an allowed move.
Precondition: There must be moves available for team. *)

val smart: Board.t -> Piece.color -> Board.t
(** [smart board team] is the new board after an ai decides which move
would be best based upon possible captures of opponent pieces, as well as
a preset strategic value for every position that is unique to the type
of a moving piece
Precondition: There must be moves available for team. *)