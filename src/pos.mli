exception InvalidCommand

type t
(* The type to represent a position on a chess board *)

val pos : int -> int -> t
(** [pos int1 int2] creates a pos at (int1, int2), where int1 is the
    x-position and int2 is the y-position *)

val fst : t -> int
val snd : t -> int

val add : t -> t -> t
(** [add pos1 pos2] is the component-wide addition of pos1 and pos2 *)

val minus : t -> t -> t
(** [minus pos1 pos2] is the component-wide subtraction of pos2 from
    pos1 *)

val multiply : t -> int -> t
(** [multiply pos1 scalar] is the scalar multiplication of pos1 with
    scalar*)

val non_zero_entry : t -> int
(** [non_zero_entry pos1] is the first non-zero entry in pos1, or 0 if
    both are 0 *)

val make_ones : t -> t
(** [make_ones pos1] makes all the non_zero entries either one or
    negative one, depending on the sign*)

val one_to_right : t
(** Represents a movement of one place to the right on the board — the
    pair (1, 0) *)

val one_to_left : t
(** Represents a movement of one place to the left on the vaord - the
    pait (-1, 0) *)

val one_up : t
(** Represents a movement of up one place on the board — the pair (0, 1) *)

val one_down : t
(** Represents a movement of one place down on the board - the pair (0,
    \-1) *)

val get_all_pos : t list
(** [get_all_pos] is every possible position on the chess board *)
